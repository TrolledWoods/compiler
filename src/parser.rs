use crate::ast;
use crate::compilation_manager::{CompileManager, Dependencies, Identifier};
use crate::error::{CompileError, ErrorPrintingData};
use crate::keyword::Keyword;
use crate::lexer::{BracketKind, Lexer, LexerError, SourcePos, TextPos, Token, TokenKind};
use crate::namespace::{NamespaceError, NamespaceId};
use crate::operator;
use crate::string_pile::TinyString;
use crate::types::{CollectionDefKind, FunctionHeader, PrimitiveKind, TypeDef, TypeDefKind};
use std::convert::TryInto;
use std::fmt::{self, Display, Formatter};
use std::path::Path;
use std::sync::Arc;

pub struct Parser<'a> {
	pub manager: Arc<CompileManager>,
	pub file: TinyString,
	pub tokens: Lexer<'a>,
}

impl Parser<'_> {
	fn eat_token(&mut self) -> Result<Option<Token>, ParseError> {
		let token = self.tokens.eat_token()?;
		Ok(token)
	}

	fn peek_token(&mut self, n: usize) -> Result<Option<Token>, ParseError> {
		Ok(self.tokens.peek_token(n)?)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParsingActivity {
	Struct,
	StructMember,
	Type,
	OffloadedType,
	Tuple,
	FunctionPtr,
	Constant,
	ConstantValue,
	Namespace,
	LoadNamespace,
	TypeDef,
	Expression,
	Block,
	Let,
}

impl Display for ParsingActivity {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		use ParsingActivity::*;
		match self {
			Struct => write!(f, "struct"),
			StructMember => write!(f, "struct member"),
			Type => write!(f, "type"),
			OffloadedType => write!(f, "offloaded type"),
			Tuple => write!(f, "tuple"),
			FunctionPtr => write!(f, "function pointer"),
			Constant => write!(f, "constant"),
			ConstantValue => write!(f, "constant value"),
			Namespace => write!(f, "namespace"),
			LoadNamespace => write!(f, "external namespace load"),
			Expression => write!(f, "expression"),
			TypeDef => write!(f, "type definition"),
			Block => write!(f, "block"),
			Let => write!(f, "let"),
		}
	}
}

#[derive(Debug)]
pub enum ParseError {
	UnexpectedToken(UnexpectedTokenError),
	Lexer(LexerError),
	Namespace(NamespaceError),
	IoError(String, std::io::Error),
}

impl CompileError for ParseError {
	fn get_printing_data(self) -> ErrorPrintingData {
		match self {
			ParseError::UnexpectedToken(error) => {
				let message = format!("Unexpected token while parsing {}", error.activity);

				let pos = match error.got {
					Some(token) => SourcePos {
						file: error.file,
						start: token.start,
						end: token.end,
					},
					None => SourcePos {
						file: error.file,
						start: TextPos::end_of_file(),
						end: TextPos::end_of_file(),
					},
				};

				let mut expected = format!("Expected ");
				for (i, value) in error.expected.iter().enumerate() {
					let addition = if i > 0 {
						format!(", {}", value)
					} else {
						format!("{}", value)
					};

					expected.push_str(&addition);
				}

				ErrorPrintingData::new(message).problem(pos, expected)
			}
			ParseError::Lexer(error) => error.get_printing_data(),
			ParseError::Namespace(error) => error.get_printing_data(),
			ParseError::IoError(file, error) => ErrorPrintingData::new(format!(
				"Error loading file '{}', \n io error: {}",
				file, error
			)),
		}
	}
}

impl From<UnexpectedTokenError> for ParseError {
	fn from(err: UnexpectedTokenError) -> ParseError {
		ParseError::UnexpectedToken(err)
	}
}

impl From<LexerError> for ParseError {
	fn from(err: LexerError) -> ParseError {
		ParseError::Lexer(err)
	}
}

impl From<NamespaceError> for ParseError {
	fn from(err: NamespaceError) -> ParseError {
		ParseError::Namespace(err)
	}
}

#[derive(Debug)]
pub struct UnexpectedTokenError {
	pub file: TinyString,
	pub activity: ParsingActivity,
	pub expected: Vec<ExpectedValue>,
	pub got: Option<Token>,
}

#[derive(Debug)]
pub enum ExpectedValue {
	Identifier,
	FileNamespace,
	Type,
	ConstantDefinitionValue,
	Expression,

	Kind(TokenKind),

	Keyword(Keyword),
	ClosingBracket(BracketKind),
	OpeningBracket(BracketKind),

	UnnamedListEntry,
	NamedListEntry,
}

impl Display for ExpectedValue {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		use ExpectedValue::*;
		match self {
			Identifier => write!(f, "identifier"),
			FileNamespace => write!(f, "namespaced file"),
			Type => write!(f, "type"),
			ConstantDefinitionValue => write!(f, "constant definition value"),
			Expression => write!(f, "expression"),
			Kind(kind) => write!(f, "kind '{:?}'", kind),
			Keyword(keyword) => write!(f, "keyword '{:?}'", keyword),
			ClosingBracket(kind) => write!(f, "closing {:?}", kind),
			OpeningBracket(kind) => write!(f, "opening {:?}", kind),
			UnnamedListEntry => write!(f, "unnamed list entry"),
			NamedListEntry => write!(f, "named list entry"),
		}
	}
}

pub fn parse_file(
	file: &Path,
	manager: Arc<CompileManager>,
	// TODO: Pass thread pool / task pool into here
	namespace_id: NamespaceId,
) -> Result<(), ParseError> {
	let input = match std::fs::read_to_string(&file) {
		Ok(val) => val,
		Err(io_error) => return Err(ParseError::IoError(file.to_str().unwrap().into(), io_error)),
	};
	let lexer = Lexer::new(
		file.to_str()
			.expect("Cannot turn file path into string")
			.into(),
		&input,
	);
	let mut parser = Parser {
		manager,
		file: file
			.to_str()
			.expect("String conversion not possible :<")
			.into(),
		tokens: lexer,
	};

	parse_namespace(&mut parser, false, namespace_id)
}

/// Just parses a namespace.
/// No opening bracket is expected, but if the
/// `in_block` parameter is true, we expect a
/// closing bracket.
pub fn parse_namespace(
	parser: &mut Parser<'_>,
	in_block: bool,
	id: NamespaceId,
) -> Result<(), ParseError> {
	while let Some(token) = parser.peek_token(0)? {
		if let Token {
			kind: TokenKind::ClosingBracket(BracketKind::Curly),
			..
		} = token
		{
			if in_block {
				parser.eat_token()?;
				return Ok(());
			}
		}

		match token {
			Token {
				kind: TokenKind::Keyword(Keyword::TypeDef),
				..
			} => {
				parse_type_def(parser, id)?;
			}
			Token {
				kind: TokenKind::Keyword(Keyword::Const),
				..
			} => {
				parse_constant_def(parser, id)?;
			}

			_ => unimplemented!(),
		}
	}

	if !in_block {
		Ok(())
	} else {
		Err(UnexpectedTokenError {
			file: parser.file,
			activity: ParsingActivity::Namespace,
			expected: vec![ExpectedValue::ClosingBracket(BracketKind::Curly)],
			got: None,
		}
		.into())
	}
}

fn parse_expression(
	parser: &mut Parser<'_>,
	id: NamespaceId,
) -> Result<ast::ExpressionDef, ParseError> {
	parse_expression_rec(parser, 0, id)
}

fn parse_expression_rec(
	parser: &mut Parser<'_>,
	priority: u8,
	id: NamespaceId,
) -> Result<ast::ExpressionDef, ParseError> {
	let mut expression = Some(parse_value(parser, id)?);

	loop {
		match parser.peek_token(0)? {
			Some(Token {
				kind: TokenKind::Operator(op),
				start,
				end,
			}) => {
				let op_priority: u8 = operator::priority(op).into();

				if op_priority > priority {
					parser.eat_token()?;

					let left = expression.take().unwrap();
					let right = parse_expression_rec(parser, op_priority, id)?;

					expression = Some(ast::ExpressionDef {
						kind: ast::ExpressionDefKind::Operator(op, vec![left, right]),
						pos: SourcePos {
							file: parser.file,
							start,
							end,
						},
					});
				} else {
					break Ok(expression.unwrap());
				}
			}
			_ => break Ok(expression.unwrap()),
		}
	}
}

fn parse_value(parser: &mut Parser<'_>, id: NamespaceId) -> Result<ast::ExpressionDef, ParseError> {
	let mut value = Some(parse_non_func_call_value(parser, id)?);

	// Parse function calls
	while let Some(Token {
		kind: TokenKind::OpeningBracket(BracketKind::Brack),
		..
	}) = parser.peek_token(0)?
	{
		let (args_pos, args) = parse_list(
			parser,
			ListGrammar::square(),
			|parser| parse_expression(parser, id),
			ParsingActivity::Expression,
		)?;

		let old_value = value.take().unwrap();
		value = Some(ast::ExpressionDef {
			kind: ast::ExpressionDefKind::FunctionCall {
				function: Box::new(old_value),
				args: args,
			},
			pos: args_pos,
		});
	}

	Ok(value.unwrap())
}

fn parse_non_func_call_value(
	parser: &mut Parser<'_>,
	id: NamespaceId,
) -> Result<ast::ExpressionDef, ParseError> {
	match parser.peek_token(0)? {
		Some(Token {
			kind: TokenKind::OpeningBracket(BracketKind::Paren),
			start,
			end,
		}) => {
			let sub_namespace_id = parser
				.manager
				.namespace_manager
				.create_anonymous_namespace(id);

			let (statements, expression) = parse_block(parser, sub_namespace_id)?;

			if statements.len() == 0 {
				if let Some(expression) = expression {
					Ok(expression)
				} else {
					// An empty block ;D
					Ok(ast::ExpressionDef {
						pos: SourcePos {
							file: parser.file,
							start,
							end,
						},
						kind: ast::ExpressionDefKind::Block(vec![], None),
					})
				}
			} else {
				Ok(ast::ExpressionDef {
					pos: SourcePos {
						file: parser.file,
						start,
						end,
					},
					kind: ast::ExpressionDefKind::Block(
						statements,
						expression.map(|v| Box::new(v)),
					),
				})
			}
		}
		Some(Token {
			kind: TokenKind::OpeningBracket(BracketKind::Brack),
			start,
			end,
		}) => {
			// Array type: [] i32
			// Function type: [i32] -> []

			let (list_pos, list) = parse_unique_named_or_unnamed_list(
				parser,
				ListGrammar::square(),
				|parser| parse_expression(parser, id),
				|parser| parse_type(parser),
				ParsingActivity::Block,
			)?;

			match parser.peek_token(0)? {
				Some(Token {
					kind: TokenKind::ReturnArrow,
					..
				}) => {
					let args = match list {
						UniqueListKind::Empty => vec![],
						UniqueListKind::Named(elements) => elements,
						UniqueListKind::Unnamed(_) => {
							panic!("TODO: Function definition without arg names")
						}
					};

					parser.eat_token()?;

					// Parse return args
					let (return_pos, returns) = parse_list(
						parser,
						ListGrammar::square(),
						|parser| parse_type(parser),
						ParsingActivity::Block,
					)?;

					// Parse function body
					let sub_id = parser
						.manager
						.namespace_manager
						.create_anonymous_namespace(id);
					let content = parse_value(parser, sub_id)?;

					Ok(ast::ExpressionDef {
						kind: ast::ExpressionDefKind::Function(args, returns, Box::new(content)),
						pos: list_pos,
					})
				}
				Some(Token {
					kind: TokenKind::OpeningBracket(BracketKind::Paren),
					..
				}) => {
					let args = match list {
						UniqueListKind::Empty => vec![],
						UniqueListKind::Named(elements) => elements,
						UniqueListKind::Unnamed(_) => {
							panic!("TODO: Function definition without arg names")
						}
					};

					// Function without return args
					let sub_id = parser
						.manager
						.namespace_manager
						.create_anonymous_namespace(id);
					let content = parse_value(parser, sub_id)?;

					Ok(ast::ExpressionDef {
						kind: ast::ExpressionDefKind::Function(args, vec![], Box::new(content)),
						pos: list_pos,
					})
				}
				_ => {
					let members = match list {
						UniqueListKind::Empty => vec![],
						UniqueListKind::Unnamed(elements) => elements,
						UniqueListKind::Named(_) => {
							panic!("TODO: Array definition with arg names is not allowed")
						}
					};
					//
					// This is an array
					Ok(ast::ExpressionDef {
						kind: ast::ExpressionDefKind::Array(members),
						pos: list_pos,
					})
				}
			}
		}
		Some(Token {
			kind: TokenKind::OpeningBracket(BracketKind::Curly),
			start,
			end,
		}) => {
			// Either a struct, a tuple or a nil
			let (list_pos, list) = parse_named_or_unnamed_list(
				parser,
				ListGrammar::curly(),
				|parser| parse_expression(parser, id),
				ParsingActivity::Block,
			)?;

			match list {
				ListKind::Empty => Ok(ast::ExpressionDef {
					pos: list_pos,
					kind: ast::ExpressionDefKind::Collection(ast::CollectionDefKind::Unnamed(
						vec![],
					)),
				}),
				ListKind::Named(members) => Ok(ast::ExpressionDef {
					pos: list_pos,
					kind: ast::ExpressionDefKind::Collection(ast::CollectionDefKind::Named(
						members,
					)),
				}),
				ListKind::Unnamed(members) => Ok(ast::ExpressionDef {
					pos: list_pos,
					kind: ast::ExpressionDefKind::Collection(ast::CollectionDefKind::Unnamed(
						members,
					)),
				}),
			}
		}
		Some(Token {
			kind: TokenKind::Operator(op),
			start,
			end,
		}) => {
			parser.eat_token()?;
			let arg = parse_value(parser, id)?;
			Ok(ast::ExpressionDef {
				pos: SourcePos {
					file: parser.file,
					start,
					end,
				},
				kind: ast::ExpressionDefKind::UnaryOperator(op, Box::new(arg)),
			})
		}
		Some(Token {
			kind: TokenKind::IntLiteral(value),
			start,
			end,
		}) => {
			parser.eat_token()?;
			Ok(ast::ExpressionDef {
				pos: SourcePos {
					file: parser.file,
					start: start,
					end: end,
				},
				kind: ast::ExpressionDefKind::IntLiteral(value),
			})
		}
		Some(Token {
			kind: TokenKind::FloatLiteral(value),
			start,
			end,
		}) => {
			parser.eat_token()?;
			Ok(ast::ExpressionDef {
				pos: SourcePos {
					file: parser.file,
					start: start,
					end: end,
				},
				kind: ast::ExpressionDefKind::FloatLiteral(value),
			})
		}
		Some(Token {
			kind: TokenKind::StringLiteral(value),
			start,
			end,
		}) => {
			parser.eat_token()?;
			Ok(ast::ExpressionDef {
				pos: SourcePos {
					file: parser.file,
					start: start,
					end: end,
				},
				kind: ast::ExpressionDefKind::StringLiteral(value),
			})
		}
		Some(Token {
			kind: TokenKind::Identifier(name),
			start,
			end,
		}) => {
			parser.eat_token()?;

			Ok(ast::ExpressionDef {
				pos: SourcePos {
					file: parser.file,
					start: start,
					end: end,
				},
				kind: ast::ExpressionDefKind::Offload(name),
			})
		}
		c => Err(UnexpectedTokenError {
			file: parser.file,
			activity: ParsingActivity::Expression,
			expected: vec![ExpectedValue::Expression],
			got: c,
		}
		.into()),
	}
}

fn parse_block(
	parser: &mut Parser<'_>,
	id: NamespaceId,
) -> Result<(Vec<ast::StatementDef>, Option<ast::ExpressionDef>), ParseError> {
	parse_kind(
		parser,
		&TokenKind::OpeningBracket(BracketKind::Paren),
		ParsingActivity::Block,
	)?;

	let mut statements = Vec::new();
	while let Some(token) = parser.peek_token(0)? {
		match token {
			Token {
				kind: TokenKind::ClosingBracket(BracketKind::Paren),
				..
			} => {
				parser.eat_token()?;
				return Ok((statements, None));
			}
			Token {
				kind: TokenKind::Keyword(Keyword::Let),
				start,
				end,
			} => {
				parser.eat_token()?;

				let name = parse_identifier(parser, ParsingActivity::Let)?;

				let type_ =
					if let Some(declaration) = try_parse_kind(parser, &TokenKind::Declaration)? {
						Some(parse_type(parser)?)
					} else {
						None
					};

				parse_kind(
					parser,
					&TokenKind::AssignmentOperator(""),
					ParsingActivity::Let,
				)?;

				let expr = parse_expression(parser, id)?;

				parse_kind(parser, &TokenKind::Terminator, ParsingActivity::Let)?;

				statements.push(ast::StatementDef::Declaration(name, type_, expr));
			}
			Token {
				kind: TokenKind::Keyword(Keyword::TypeDef),
				..
			} => {
				parse_type_def(parser, id)?;
			}
			Token {
				kind: TokenKind::Keyword(Keyword::Const),
				..
			} => {
				parse_constant_def(parser, id)?;
			}
			// The last resort is to parse an expression
			_ => {
				let expression = parse_expression(parser, id)?;

				match parser.eat_token()? {
					Some(Token {
						kind: TokenKind::AssignmentOperator(op),
						start,
						end,
					}) => {
						let right = parse_expression(parser, id)?;

						parse_kind(parser, &TokenKind::Terminator, ParsingActivity::Block)?;

						statements.push(ast::StatementDef::Assignment(expression, op, right));
					}
					Some(Token {
						kind: TokenKind::ClosingBracket(BracketKind::Paren),
						..
					}) => {
						return Ok((statements, Some(expression)));
					}
					Some(Token {
						kind: TokenKind::Terminator,
						..
					}) => {
						statements.push(ast::StatementDef::Expression(expression));
					}
					_ => panic!("TODO: Invalid token after expression in block"),
				}
			}
		}
	}

	Err(UnexpectedTokenError {
		file: parser.file,
		activity: ParsingActivity::Block,
		expected: vec![ExpectedValue::ClosingBracket(BracketKind::Curly)],
		got: None,
	}
	.into())
}

fn parse_constant_def(parser: &mut Parser<'_>, id: NamespaceId) -> Result<(), ParseError> {
	parse_keyword(parser, Keyword::Const, ParsingActivity::Constant)?;

	let identifier = parse_identifier(parser, ParsingActivity::Constant)?;

	parse_kind(
		parser,
		&TokenKind::AssignmentOperator(""),
		ParsingActivity::Constant,
	)?;

	let expression = parse_expression(parser, id)?;
	expression.pretty_print(0);
	println!("");

	parse_kind(parser, &TokenKind::Terminator, ParsingActivity::Constant)?;

	Ok(())
}

fn parse_kind(
	parser: &mut Parser<'_>,
	kind: &TokenKind,
	doing: ParsingActivity,
) -> Result<Token, ParseError> {
	let token = parser.eat_token()?;
	match &token {
		Some(Token { kind: kind_, .. }) if kind_ == kind => Ok(token.unwrap()),
		_ => Err(UnexpectedTokenError {
			file: parser.file,
			activity: doing,
			expected: vec![ExpectedValue::Kind(kind.clone())],
			got: token,
		}
		.into()),
	}
}

fn parse_keyword(
	parser: &mut Parser<'_>,
	keyword: Keyword,
	doing: ParsingActivity,
) -> Result<Token, ParseError> {
	let token = parser.eat_token()?;
	match &token {
		Some(Token {
			kind: TokenKind::Keyword(k),
			..
		}) if *k == keyword => Ok(token.unwrap()),
		_ => Err(UnexpectedTokenError {
			file: parser.file,
			activity: doing,
			expected: vec![ExpectedValue::Keyword(keyword)],
			got: token,
		}
		.into()),
	}
}

fn try_parse_kind(parser: &mut Parser<'_>, kind: &TokenKind) -> Result<Option<Token>, ParseError> {
	let peeked_token = parser.peek_token(0)?;
	match &peeked_token {
		Some(Token { kind: kind_, .. }) if kind_ == kind => {
			parser.eat_token()?;
			Ok(peeked_token)
		}
		_ => Ok(None),
	}
}

/// Will eat a token and return a Some(Token) if the next token is the given keyword,
/// if the next token isn't the given keyword, it will not do anything.
pub fn try_parse_keyword(
	parser: &mut Parser<'_>,
	keyword: Keyword,
) -> Result<Option<Token>, ParseError> {
	let peeked_token = parser.peek_token(0)?;
	match peeked_token {
		Some(Token {
			kind: TokenKind::Keyword(k),
			..
		}) if k == keyword => {
			parser.eat_token()?;
			Ok(peeked_token)
		}
		_ => Ok(None),
	}
}

pub fn parse_identifier(
	parser: &mut Parser<'_>,
	doing: ParsingActivity,
) -> Result<Identifier, ParseError> {
	let token = parser.eat_token()?;
	match &token {
		Some(Token {
			kind: TokenKind::Identifier(name),
			..
		}) => Ok(Identifier {
			data: *name,
			pos: SourcePos::from_token(&token.unwrap(), parser.file),
		}),
		_ => Err(UnexpectedTokenError {
			file: parser.file,
			activity: doing,
			expected: vec![ExpectedValue::Identifier],
			got: token,
		}
		.into()),
	}
}

// Doesn't have to return anything, as it adds a compilation unit
// to the CompilationManager anyway
pub fn parse_type_def(
	parser: &mut Parser<'_>,
	namespace_id: NamespaceId,
) -> Result<(), ParseError> {
	parse_keyword(parser, Keyword::TypeDef, ParsingActivity::TypeDef)?;

	let identifier = parse_identifier(parser, ParsingActivity::TypeDef)?;

	parse_kind(
		parser,
		&TokenKind::AssignmentOperator(""),
		ParsingActivity::TypeDef,
	)?;

	let type_def = parse_type(parser)?;
	parser
		.manager
		.insert_named_type(namespace_id, identifier, type_def)?;

	parse_kind(parser, &TokenKind::Terminator, ParsingActivity::TypeDef)?;

	Ok(())
}

type ListKind<V> = UniqueListKind<V, V>;

enum UniqueListKind<N, U> {
	Empty,
	Named(Vec<(Identifier, N)>),
	Unnamed(Vec<U>),
}

fn parse_unique_named_or_unnamed_list<N, U>(
	parser: &mut Parser,
	grammar: ListGrammar,
	mut parse_unnamed_value: impl FnMut(&mut Parser) -> Result<U, ParseError>,
	mut parse_named_value: impl FnMut(&mut Parser) -> Result<N, ParseError>,
	activity: ParsingActivity,
) -> Result<(SourcePos, UniqueListKind<N, U>), ParseError> {
	let start = parse_kind(parser, &grammar.start, activity)?.start;

	let mut values = UniqueListKind::Empty;
	loop {
		match (parser.peek_token(0)?, parser.peek_token(1)?) {
			(Some(Token { kind, end, .. }), _) if kind == grammar.end => {
				// This is the end of the list
				return Ok((
					SourcePos {
						file: parser.file,
						start,
						end,
					},
					values,
				));
			}
			(
				Some(Token {
					kind: TokenKind::Identifier(name),
					start,
					end,
				}),
				Some(Token {
					kind: TokenKind::Declaration,
					..
				}),
			) => {
				// This is a named entry, because of the "name: " structure
				let owned_values = std::mem::replace(&mut values, UniqueListKind::Empty);
				let mut members = match owned_values {
					UniqueListKind::Empty => vec![],
					UniqueListKind::Named(members) => members,
					UniqueListKind::Unnamed(_) => {
						return Err(UnexpectedTokenError {
							file: parser.file,
							activity: activity,
							expected: vec![ExpectedValue::UnnamedListEntry],
							got: parser.peek_token(0)?,
						}
						.into())
					}
				};

				parser.eat_token()?;
				parser.eat_token()?;

				let value = parse_named_value(parser)?;
				members.push((
					Identifier {
						data: name,
						pos: SourcePos {
							file: parser.file,
							start,
							end,
						},
					},
					value,
				));

				values = UniqueListKind::Named(members);
			}
			_ => {
				// This is an unnamed entry, because it doesn't have the "name: "
				// structure
				let owned_values = std::mem::replace(&mut values, UniqueListKind::Empty);
				let mut members = match owned_values {
					UniqueListKind::Empty => vec![],
					UniqueListKind::Unnamed(members) => members,
					UniqueListKind::Named(_) => {
						return Err(UnexpectedTokenError {
							file: parser.file,
							activity: activity,
							expected: vec![ExpectedValue::NamedListEntry],
							got: parser.peek_token(0)?,
						}
						.into())
					}
				};

				let value = parse_unnamed_value(parser)?;
				members.push(value);

				values = UniqueListKind::Unnamed(members);
			}
		}

		// Parse a separator or a terminator
		match parser.eat_token()? {
			Some(Token { kind, end, .. }) if kind == grammar.end => {
				return Ok((
					SourcePos {
						file: parser.file,
						start,
						end,
					},
					values,
				));
			}
			Some(Token { kind, end, .. }) if kind == grammar.separator => (),
			c => {
				return Err(UnexpectedTokenError {
					file: parser.file,
					activity,
					expected: vec![
						ExpectedValue::Kind(grammar.end),
						ExpectedValue::Kind(grammar.separator),
					],
					got: c,
				}
				.into())
			}
		}
	}
}

fn parse_named_or_unnamed_list<V>(
	parser: &mut Parser,
	grammar: ListGrammar,
	mut parse_value: impl FnMut(&mut Parser) -> Result<V, ParseError>,
	activity: ParsingActivity,
) -> Result<(SourcePos, ListKind<V>), ParseError> {
	let start = match parser.peek_token(0)? {
		Some(Token { kind, start, .. }) if kind == grammar.start => start,
		_ => {
			return Err(UnexpectedTokenError {
				file: parser.file,
				activity,
				expected: vec![ExpectedValue::Kind(grammar.start)],
				got: parser.peek_token(0)?,
			}
			.into())
		}
	};

	// Determine wether it is named or not
	if let Some(Token {
		kind: TokenKind::Terminator,
		..
	}) = parser.peek_token(1)?
	{
		parser.eat_token()?;
		let terminator = parser.eat_token()?.unwrap();
		Ok((
			SourcePos {
				file: parser.file,
				start,
				end: terminator.end,
			},
			ListKind::Empty,
		))
	} else if let Some(Token {
		kind: TokenKind::Declaration,
		..
	}) = parser.peek_token(2)?
	{
		parse_named_list(parser, grammar, parse_value, activity)
			.map(|(pos, list)| (pos, ListKind::Named(list)))
	} else {
		parse_list(parser, grammar, parse_value, activity)
			.map(|(pos, list)| (pos, ListKind::Unnamed(list)))
	}
}

/// Parses a named list.
/// The syntax of a named list is:
/// name_1: [value],
/// name_2: [value],
/// [terminator].
/// The terminator token will also
/// be "eaten"
fn parse_named_list<V>(
	parser: &mut Parser,
	grammar: ListGrammar,
	mut parse_value: impl FnMut(&mut Parser) -> Result<V, ParseError>,
	activity: ParsingActivity,
) -> Result<(SourcePos, Vec<(Identifier, V)>), ParseError> {
	let start = if let Some(start) = try_parse_kind(parser, &grammar.start)? {
		start.start
	} else {
		return Err(UnexpectedTokenError {
			file: parser.file,
			activity,
			expected: vec![ExpectedValue::Kind(grammar.start)],
			got: parser.peek_token(0)?,
		}
		.into());
	};

	// Read the members
	let mut members = Vec::new();
	loop {
		if let Some(terminator) = try_parse_kind(parser, &grammar.end)? {
			return Ok((
				SourcePos {
					file: parser.file,
					start,
					end: terminator.end,
				},
				members,
			));
		}

		let name = parse_identifier(parser, activity)?;
		let _colon = parse_kind(parser, &TokenKind::Declaration, activity)?;
		let value = parse_value(parser)?;
		members.push((name, value));

		if let Some(terminator) = try_parse_kind(parser, &grammar.end)? {
			return Ok((
				SourcePos {
					file: parser.file,
					start,
					end: terminator.end,
				},
				members,
			));
		} else {
			parse_kind(parser, &grammar.separator, activity)?;
		}
	}
}

pub struct ListGrammar {
	pub start: TokenKind,
	pub separator: TokenKind,
	pub end: TokenKind,
}

impl ListGrammar {
	pub fn parenthesis() -> ListGrammar {
		ListGrammar {
			start: TokenKind::OpeningBracket(BracketKind::Paren),
			separator: TokenKind::Separator,
			end: TokenKind::ClosingBracket(BracketKind::Paren),
		}
	}

	pub fn square() -> ListGrammar {
		ListGrammar {
			start: TokenKind::OpeningBracket(BracketKind::Brack),
			separator: TokenKind::Separator,
			end: TokenKind::ClosingBracket(BracketKind::Brack),
		}
	}

	pub fn curly() -> ListGrammar {
		ListGrammar {
			start: TokenKind::OpeningBracket(BracketKind::Curly),
			separator: TokenKind::Separator,
			end: TokenKind::ClosingBracket(BracketKind::Curly),
		}
	}

	pub fn angle() -> ListGrammar {
		ListGrammar {
			start: TokenKind::Operator("<"),
			separator: TokenKind::Separator,
			end: TokenKind::Operator(">"),
		}
	}
}

fn try_parse_list<V>(
	parser: &mut Parser,
	grammar: ListGrammar,
	parse_value: impl FnMut(&mut Parser) -> Result<V, ParseError>,
	activity: ParsingActivity,
) -> Result<Option<(SourcePos, Vec<V>)>, ParseError> {
	if let Some(Token { kind, .. }) = parser.peek_token(0)? {
		if kind == grammar.start {
			Ok(Some(parse_list(parser, grammar, parse_value, activity)?))
		} else {
			Ok(None)
		}
	} else {
		Ok(None)
	}
}

fn parse_list<V>(
	parser: &mut Parser,
	grammar: ListGrammar,
	mut parse_value: impl FnMut(&mut Parser) -> Result<V, ParseError>,
	activity: ParsingActivity,
) -> Result<(SourcePos, Vec<V>), ParseError> {
	let start = if let Some(start) = try_parse_kind(parser, &grammar.start)? {
		start.start
	} else {
		return Err(UnexpectedTokenError {
			file: parser.file,
			activity,
			expected: vec![ExpectedValue::Kind(grammar.start)],
			got: parser.peek_token(0)?,
		}
		.into());
	};

	let mut members = Vec::new();
	loop {
		if let Some(terminator) = try_parse_kind(parser, &grammar.end)? {
			return Ok((
				SourcePos {
					file: parser.file,
					start,
					end: terminator.end,
				},
				members,
			));
		}

		members.push(parse_value(parser)?);

		if let Some(terminator) = try_parse_kind(parser, &grammar.end)? {
			return Ok((
				SourcePos {
					file: parser.file,
					start,
					end: terminator.end,
				},
				members,
			));
		} else {
			parse_kind(parser, &grammar.separator, activity)?;
		}
	}
}

fn parse_collection(parser: &mut Parser) -> Result<(SourcePos, CollectionDefKind), ParseError> {
	let (pos, members) = parse_named_or_unnamed_list(
		parser,
		ListGrammar::curly(),
		|parser| parse_type(parser),
		ParsingActivity::StructMember,
	)?;

	match members {
		ListKind::Empty => Ok((pos, CollectionDefKind::Unnamed(vec![]))),
		ListKind::Named(members) => {
			let members: Vec<(TinyString, TypeDef)> = members
				.into_iter()
				.map(|(name, v)| (name.data, v))
				.collect();

			Ok((pos, CollectionDefKind::Named(members)))
		}
		ListKind::Unnamed(members) => Ok((pos, CollectionDefKind::Unnamed(members))),
	}
}

fn parse_type(parser: &mut Parser) -> Result<TypeDef, ParseError> {
	match parser.peek_token(0)? {
		Some(Token {
			kind: TokenKind::ArrayWindow,
			start,
			end,
		}) => {
			// This is an array window
			parser.eat_token()?;
			let sub_type = parse_type(parser)?;

			Ok(TypeDef {
				pos: SourcePos {
					file: parser.file,
					start,
					end,
				},
				kind: TypeDefKind::ArrayWindow(Box::new(sub_type)),
			})
		}
		Some(Token {
			kind: TokenKind::DynamicArray,
			start,
			end,
		}) => {
			// This is a dynamic array!
			parser.eat_token()?;
			let sub_type = parse_type(parser)?;

			Ok(TypeDef {
				pos: SourcePos {
					file: parser.file,
					start,
					end,
				},
				kind: TypeDefKind::DynamicArray(Box::new(sub_type)),
			})
		}
		Some(Token {
			kind: TokenKind::OpeningBracket(BracketKind::Brack),
			start,
			..
		}) => {
			match parser.peek_token(1)? {
				Some(Token {
					kind: TokenKind::IntLiteral(count),
					end,
					..
				}) => {
					// This is a statically sized array
					parser.eat_token()?;
					parser.eat_token()?;

					parse_kind(
						parser,
						&TokenKind::ClosingBracket(BracketKind::Brack),
						ParsingActivity::Type,
					)?;

					let count: usize = match count.try_into() {
						Ok(v) if v > 0 => v,
						Err(_) | Ok(_) => panic!("TODO: Invalid array size number"),
					};

					let sub_type = parse_type(parser)?;

					Ok(TypeDef {
						pos: SourcePos {
							file: parser.file,
							start,
							end,
						},
						kind: TypeDefKind::StaticArray(count, Box::new(sub_type)),
					})
				}
				_ => {
					// This is a function(because it's not an array)
					let (pos, args) = parse_list(
						parser,
						ListGrammar::square(),
						|parser| parse_type(parser),
						ParsingActivity::FunctionPtr,
					)?;

					parse_kind(
						parser,
						&TokenKind::ReturnArrow,
						ParsingActivity::FunctionPtr,
					)?;

					let (return_pos, returns) = parse_list(
						parser,
						ListGrammar::square(),
						|parser| parse_type(parser),
						ParsingActivity::FunctionPtr,
					)?;

					Ok(TypeDef {
						pos: SourcePos {
							file: parser.file,
							start: pos.start,
							end: return_pos.end,
						},
						kind: TypeDefKind::FunctionPtr(FunctionHeader { args, returns }),
					})
				}
			}
		}
		Some(Token {
			kind: TokenKind::OpeningBracket(BracketKind::Curly),
			..
		}) => {
			let (pos, collection) = parse_collection(parser)?;

			match collection {
				CollectionDefKind::Unnamed(members) => Ok(TypeDef {
					pos,
					kind: TypeDefKind::Tuple(members),
				}),
				CollectionDefKind::Named(members) => Ok(TypeDef {
					pos,
					kind: TypeDefKind::Struct(members),
				}),
			}
		}
		Some(Token {
			kind: TokenKind::Operator("*"),
			start,
			..
		}) => {
			parser.eat_token()?;
			let internal_type = parse_type(parser)?;
			Ok(TypeDef {
				pos: SourcePos {
					file: parser.file,
					start: start,
					end: internal_type.pos.end,
				},
				kind: TypeDefKind::Pointer(Box::new(internal_type)),
			})
		}
		Some(Token {
			kind: TokenKind::Identifier(_),
			..
		}) => parse_offloaded_type(parser),
		c => Err(UnexpectedTokenError {
			file: parser.file,
			activity: ParsingActivity::Type,
			expected: vec![ExpectedValue::Type],
			got: c,
		}
		.into()),
	}
}

fn parse_tuple_type(
	parser: &mut Parser,

	dependencies: &mut Dependencies,
) -> Result<(SourcePos, Vec<TypeDef>), ParseError> {
	let (pos, members) = parse_list(
		parser,
		ListGrammar::parenthesis(),
		|value| parse_type(value),
		ParsingActivity::Tuple,
	)?;

	Ok((pos, members))
}

fn parse_offloaded_type(parser: &mut Parser) -> Result<TypeDef, ParseError> {
	let name = parse_identifier(parser, ParsingActivity::OffloadedType)?;

	// See if it's a primitive
	{
		let data = name.data.read();
		let primitive = match &data as &str {
			"f32" => Some(PrimitiveKind::F32),
			"f64" => Some(PrimitiveKind::F64),
			"u8" => Some(PrimitiveKind::U8),
			"u16" => Some(PrimitiveKind::U16),
			"u32" => Some(PrimitiveKind::U32),
			"u64" => Some(PrimitiveKind::U64),
			"i8" => Some(PrimitiveKind::I8),
			"i16" => Some(PrimitiveKind::I16),
			"i32" => Some(PrimitiveKind::I32),
			"i64" => Some(PrimitiveKind::I64),
			_ => None,
		};

		if let Some(primitive) = primitive {
			return Ok(TypeDef {
				pos: name.pos,
				kind: TypeDefKind::Primitive(primitive),
			});
		}
	}

	let (pos, generics) = match try_parse_list(
		parser,
		ListGrammar::angle(),
		|value| parse_type(value),
		ParsingActivity::OffloadedType,
	)? {
		Some((generics_pos, list)) => (
			SourcePos {
				file: parser.file,
				start: name.pos.start,
				end: generics_pos.end,
			},
			list,
		),
		None => (name.pos.clone(), vec![]),
	};

	Ok(TypeDef {
		pos,
		kind: TypeDefKind::Offload(name.data),
	})
}
