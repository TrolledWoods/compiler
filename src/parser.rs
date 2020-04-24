use crate::ast;
use crate::compilation_manager::{CompileManager, Dependencies, Identifier};
use crate::error::{CompileError, ErrorPrintingData};
use crate::keyword::Keyword;
use crate::lexer::{Lexer, LexerError, Literal, SourcePos, TextPos, Token, TokenKind};
use crate::namespace::{NamespaceError, NamespaceId};
use crate::operator;
use crate::string_pile::TinyString;
use crate::types::{CollectionDefKind, FunctionHeader, PrimitiveKind, TypeDef, TypeDefKind};
use std::convert::TryInto;
use std::fmt::{self, Debug, Display, Formatter};
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
	Value,
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
			Value => write!(f, "value"),
			TypeDef => write!(f, "type definition"),
			Block => write!(f, "block"),
			Let => write!(f, "let"),
		}
	}
}

#[derive(Debug)]
pub enum ParseError {
	UnexpectedToken(UnexpectedTokenError),
	InvalidStaticArraySize(SourcePos),
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
			ParseError::InvalidStaticArraySize(pos) => {
				ErrorPrintingData::new(format!("Invalid static array size"))
					.problem(pos, format!("Static array size has to be bigger than 0"))
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
	Value,

	Kind(TokenKind),

	Keyword(Keyword),
	Bracket(char),

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
			Value => write!(f, "value"),
			Kind(kind) => write!(f, "kind '{:?}'", kind),
			Keyword(keyword) => write!(f, "keyword '{:?}'", keyword),
			Bracket(c) => write!(f, "{:?}", c),
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
			kind: TokenKind::Bracket('}'),
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
		debug_err!(UnexpectedTokenError {
			file: parser.file,
			activity: ParsingActivity::Namespace,
			expected: vec![ExpectedValue::Bracket('}')],
			got: None,
		})
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
						pos: source_pos(parser, start, end),
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
	let value: Result<ast::ExpressionDef, ParseError> = match parser.peek_token(0)? {
		// A block
		Some(Token {
			kind: TokenKind::Bracket('('),
			start,
			end,
		}) => {
			let sub_namespace_id = parser
				.manager
				.namespace_manager
				.create_anonymous_namespace(id);

			let (statements, expression) = parse_block(parser, sub_namespace_id)?;

			match (statements.as_slice(), expression) {
				// No statements, so the block was only used for order of operations
				([], Some(expression)) => Ok(expression),

				// Statements exist, so we need a full block node
				(_, expression) => Ok(ast::ExpressionDef {
					pos: source_pos(parser, start, end),
					kind: ast::ExpressionDefKind::Block(
						statements,
						expression.map(|v| Box::new(v)),
					),
				}),
			}
		}
		// Either a function definition or an array definition.
		Some(Token {
			kind: TokenKind::Bracket('['),
			start,
			end,
		}) => {
			// We say it's a function if there is a '->' or a block after
			// the first brackets. I might change this later, for now though,
			// doing "[] ()" or "[] -> [] ()" for all functions is recommended.
			let (list_pos, list) = parse_named_or_unnamed_list(
				parser,
				ListGrammar::square(),
				|parser| parse_expression(parser, id),
				|parser| parse_type(parser, id),
				ParsingActivity::Block,
			)?;

			// Decide if it's an array or a function
			match parser.peek_token(0)? {
				Some(Token {
					kind: TokenKind::ReturnArrow,
					..
				})
				| Some(Token {
					kind: TokenKind::Bracket('('),
					..
				}) => {
					// A function definition!
					let args = list
						.into_named()
						.expect("TODO: Function arg list has to be named");

					// Parse return args
					let returns = if let Some(_) = try_parse_kind(parser, &TokenKind::ReturnArrow)?
					{
						let (_, returns) = parse_list(
							parser,
							ListGrammar::square(),
							|parser| parse_type(parser, id),
							ParsingActivity::Block,
						)?;

						returns
					} else {
						vec![]
					};

					// Parse body
					let sub_id = parser
						.manager
						.namespace_manager
						.create_anonymous_namespace(id);
					let content = parse_value(parser, sub_id)?;

					let (names, args): (Vec<_>, Vec<_>) = args.into_iter().unzip();

					let function_id = parser.manager.insert_function(
						id,
						list_pos.clone(),
						names,
						FunctionHeader { args, returns },
						content,
					);

					Ok(ast::ExpressionDef {
						kind: ast::ExpressionDefKind::Function(function_id),
						pos: list_pos,
					})
				}
				_ => {
					// An array
					let members = list
						.into_unnamed()
						.expect("Array cannot have named members");

					Ok(ast::ExpressionDef {
						kind: ast::ExpressionDefKind::Array(members),
						pos: list_pos,
					})
				}
			}
		}
		// A collection
		Some(Token {
			kind: TokenKind::Bracket('{'),
			start,
			end,
		}) => {
			// Either a struct, a tuple or a nil
			let (list_pos, list) = parse_named_or_unnamed_list(
				parser,
				ListGrammar::curly(),
				|parser| parse_expression(parser, id),
				|parser| parse_expression(parser, id),
				ParsingActivity::Block,
			)?;

			Ok(ast::ExpressionDef {
				pos: list_pos,
				kind: ast::ExpressionDefKind::Collection(list),
			})
		}
		// A unary operator(normal operators are dealt with by the "parse_expression" function
		Some(Token {
			kind: TokenKind::Operator(op),
			start,
			end,
		}) => {
			parser.eat_token()?;
			let arg = parse_value(parser, id)?;
			Ok(ast::ExpressionDef {
				pos: source_pos(parser, start, end),
				kind: ast::ExpressionDefKind::UnaryOperator(op, Box::new(arg)),
			})
		}
		// Literals
		Some(Token {
			kind: TokenKind::Literal(value),
			start,
			end,
		}) => {
			parser.eat_token()?;
			Ok(ast::ExpressionDef {
				pos: source_pos(parser, start, end),
				kind: ast::ExpressionDefKind::Literal(value),
			})
		}
		Some(Token {
			kind: TokenKind::Identifier(name),
			start,
			end,
		}) => {
			parser.eat_token()?;

			Ok(ast::ExpressionDef {
				pos: source_pos(parser, start, end),
				kind: ast::ExpressionDefKind::Offload(name, id),
			})
		}
		c => debug_err!(UnexpectedTokenError {
			file: parser.file,
			activity: ParsingActivity::Value,
			expected: vec![ExpectedValue::Value],
			got: c,
		}),
	};

	let mut value = Some(value?);

	// Parse function calls
	while let Some(Token {
		kind: TokenKind::Bracket('['),
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

fn parse_block(
	parser: &mut Parser<'_>,
	id: NamespaceId,
) -> Result<(Vec<ast::StatementDef>, Option<ast::ExpressionDef>), ParseError> {
	parse_kind(parser, &TokenKind::Bracket('('), ParsingActivity::Block)?;

	let mut statements = Vec::new();
	while let Some(token) = parser.peek_token(0)? {
		match token {
			Token {
				kind: TokenKind::Bracket(')'),
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
						Some(parse_type(parser, id)?)
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
						kind: TokenKind::Bracket(')'),
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
					got => {
						return debug_err!(UnexpectedTokenError {
							file: parser.file,
							activity: ParsingActivity::Block,
							expected: vec![
								ExpectedValue::Kind(TokenKind::Terminator),
								ExpectedValue::Kind(TokenKind::Bracket(')')),
								ExpectedValue::Kind(TokenKind::AssignmentOperator("")),
								ExpectedValue::Kind(TokenKind::Bracket('[')),
							],
							got,
						})
					}
				}
			}
		}
	}

	debug_err!(UnexpectedTokenError {
		file: parser.file,
		activity: ParsingActivity::Block,
		expected: vec![ExpectedValue::Bracket('}')],
		got: None,
	})
}

fn parse_constant_def(parser: &mut Parser<'_>, id: NamespaceId) -> Result<(), ParseError> {
	parse_keyword(parser, Keyword::Const, ParsingActivity::Constant)?;

	let identifier = parse_identifier(parser, ParsingActivity::Constant)?;

	parse_kind(parser, &TokenKind::Declaration, ParsingActivity::Constant)?;

	let type_def = parse_type(parser, id)?;

	parse_kind(
		parser,
		&TokenKind::AssignmentOperator(""),
		ParsingActivity::Constant,
	)?;

	let expression = parse_expression(parser, id)?;
	expression.pretty_print(0);
	println!("");

	parse_kind(parser, &TokenKind::Terminator, ParsingActivity::Constant)?;

	parser
		.manager
		.insert_constant(id, identifier, expression, Some(type_def));

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
		_ => debug_err!(UnexpectedTokenError {
			file: parser.file,
			activity: doing,
			expected: vec![ExpectedValue::Kind(kind.clone())],
			got: token,
		}),
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
		_ => debug_err!(UnexpectedTokenError {
			file: parser.file,
			activity: doing,
			expected: vec![ExpectedValue::Keyword(keyword)],
			got: token,
		}),
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
		_ => debug_err!(UnexpectedTokenError {
			file: parser.file,
			activity: doing,
			expected: vec![ExpectedValue::Identifier],
			got: token,
		}),
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

	let type_def = parse_type(parser, namespace_id)?;
	parser
		.manager
		.insert_named_type(namespace_id, identifier, type_def)?;

	parse_kind(parser, &TokenKind::Terminator, ParsingActivity::TypeDef)?;

	Ok(())
}

pub enum ListKind<N, U> {
	Empty,
	Named(Vec<(Identifier, N)>),
	Unnamed(Vec<U>),
}

impl<N: Debug, U: Debug> Debug for ListKind<N, U> {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			ListKind::Empty => write!(f, "{}", "{}"),
			ListKind::Named(members) => {
				write!(f, "{}", "{")?;
				for (i, (name, member)) in members.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}: {:?}", name.data, member)?;
				}
				write!(f, "{}", "}")?;
				Ok(())
			}
			ListKind::Unnamed(members) => {
				write!(f, "{}", "{")?;
				for (i, member) in members.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{:?}", member)?;
				}
				write!(f, "{}", "}")?;
				Ok(())
			}
		}
	}
}

impl<N, U> ListKind<N, U> {
	pub fn into_named(self) -> Option<Vec<(Identifier, N)>> {
		match self {
			ListKind::Empty => Some(vec![]),
			ListKind::Named(elements) => Some(elements),
			ListKind::Unnamed(_) => None,
		}
	}

	pub fn into_unnamed(self) -> Option<Vec<U>> {
		match self {
			ListKind::Empty => Some(vec![]),
			ListKind::Named(_) => None,
			ListKind::Unnamed(elements) => Some(elements),
		}
	}
}

fn parse_named_or_unnamed_list<N, U>(
	parser: &mut Parser,
	grammar: ListGrammar,
	mut parse_unnamed_value: impl FnMut(&mut Parser) -> Result<U, ParseError>,
	mut parse_named_value: impl FnMut(&mut Parser) -> Result<N, ParseError>,
	activity: ParsingActivity,
) -> Result<(SourcePos, ListKind<N, U>), ParseError> {
	let start = parse_kind(parser, &grammar.start, activity)?.start;

	let mut values = ListKind::Empty;
	loop {
		match (parser.peek_token(0)?, parser.peek_token(1)?) {
			(Some(Token { kind, end, .. }), _) if kind == grammar.end => {
				// This is the end of the list
				parser.eat_token()?;
				return Ok((source_pos(parser, start, end), values));
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
				let owned_values = std::mem::replace(&mut values, ListKind::Empty);
				let mut members = owned_values.into_named().ok_or_else(|| {
					err!(UnexpectedTokenError {
						file: parser.file,
						activity: activity,
						expected: vec![ExpectedValue::UnnamedListEntry],
						got: parser.peek_token(0).unwrap(),
					})
				})?;

				parser.eat_token()?;
				parser.eat_token()?;

				let value = parse_named_value(parser)?;
				members.push((
					Identifier {
						data: name,
						pos: source_pos(parser, start, end),
					},
					value,
				));

				values = ListKind::Named(members);
			}
			_ => {
				// This is an unnamed entry, because it doesn't have the "name: "
				// structure
				let owned_values = std::mem::replace(&mut values, ListKind::Empty);
				let mut members = owned_values.into_unnamed().ok_or_else(|| {
					err!(UnexpectedTokenError {
						file: parser.file,
						activity: activity,
						expected: vec![ExpectedValue::NamedListEntry],
						got: parser.peek_token(0).unwrap(),
					})
				})?;

				let value = parse_unnamed_value(parser)?;
				members.push(value);

				values = ListKind::Unnamed(members);
			}
		}

		// Parse a separator or a terminator
		match parser.eat_token()? {
			Some(Token { kind, end, .. }) if kind == grammar.end => {
				return Ok((source_pos(parser, start, end), values));
			}
			Some(Token { kind, end, .. }) if kind == grammar.separator => {}
			c => {
				return debug_err!(UnexpectedTokenError {
					file: parser.file,
					activity,
					expected: vec![
						ExpectedValue::Kind(grammar.end),
						ExpectedValue::Kind(grammar.separator),
					],
					got: c,
				});
			}
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
			start: TokenKind::Bracket('('),
			separator: TokenKind::Separator,
			end: TokenKind::Bracket(')'),
		}
	}

	pub fn square() -> ListGrammar {
		ListGrammar {
			start: TokenKind::Bracket('['),
			separator: TokenKind::Separator,
			end: TokenKind::Bracket(']'),
		}
	}

	pub fn curly() -> ListGrammar {
		ListGrammar {
			start: TokenKind::Bracket('{'),
			separator: TokenKind::Separator,
			end: TokenKind::Bracket('}'),
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
	let start = parse_kind(parser, &grammar.start, activity)?.start;

	let mut members = Vec::new();
	loop {
		// Check if the list ended
		if let Some(terminator) = try_parse_kind(parser, &grammar.end)? {
			return Ok((source_pos(parser, start, terminator.end), members));
		}

		// Parse the member
		members.push(parse_value(parser)?);

		// Check for a separator / list end
		if let Some(terminator) = try_parse_kind(parser, &grammar.end)? {
			return Ok((source_pos(parser, start, terminator.end), members));
		} else {
			parse_kind(parser, &grammar.separator, activity)?;
		}
	}
}

fn parse_collection(
	parser: &mut Parser,
	id: NamespaceId,
) -> Result<(SourcePos, CollectionDefKind), ParseError> {
	let (pos, members) = parse_named_or_unnamed_list(
		parser,
		ListGrammar::curly(),
		|parser| parse_type(parser, id),
		|parser| parse_type(parser, id),
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

fn parse_type(parser: &mut Parser, id: NamespaceId) -> Result<TypeDef, ParseError> {
	match parser.peek_token(0)? {
		Some(Token {
			kind: TokenKind::ArrayWindow,
			start,
			end,
		}) => {
			// This is an array window
			parser.eat_token()?;
			let sub_type = parse_type(parser, id)?;

			Ok(TypeDef {
				pos: source_pos(parser, start, end),
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
			let sub_type = parse_type(parser, id)?;

			Ok(TypeDef {
				pos: source_pos(parser, start, end),
				kind: TypeDefKind::DynamicArray(Box::new(sub_type)),
			})
		}
		Some(Token {
			kind: TokenKind::Bracket('['),
			start,
			..
		}) => {
			match parser.peek_token(1)? {
				Some(Token {
					kind: TokenKind::Literal(Literal::Int(count)),
					start: num_start,
					end,
				}) => {
					// This is a statically sized array
					parser.eat_token()?;
					parser.eat_token()?;

					parse_kind(parser, &TokenKind::Bracket(']'), ParsingActivity::Type)?;

					let count: usize = match count.try_into() {
						Ok(v) if v > 0 => v,
						Err(_) | Ok(_) => {
							return debug_err!(ParseError::InvalidStaticArraySize(source_pos(
								parser, num_start, end
							)))
						}
					};

					let sub_type = parse_type(parser, id)?;

					Ok(TypeDef {
						pos: source_pos(parser, start, end),
						kind: TypeDefKind::StaticArray(count, Box::new(sub_type)),
					})
				}
				_ => {
					// This is a function(because it's not an array)
					let (pos, args) = parse_list(
						parser,
						ListGrammar::square(),
						|parser| parse_type(parser, id),
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
						|parser| parse_type(parser, id),
						ParsingActivity::FunctionPtr,
					)?;

					Ok(TypeDef {
						pos: source_pos(parser, pos.start, return_pos.end),
						kind: TypeDefKind::FunctionPtr(FunctionHeader { args, returns }),
					})
				}
			}
		}
		Some(Token {
			kind: TokenKind::Bracket('{'),
			..
		}) => {
			let (pos, collection) = parse_collection(parser, id)?;

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
			let internal_type = parse_type(parser, id)?;
			Ok(TypeDef {
				pos: source_pos(parser, start, internal_type.pos.end),
				kind: TypeDefKind::Pointer(Box::new(internal_type)),
			})
		}
		Some(Token {
			kind: TokenKind::Identifier(_),
			..
		}) => parse_offloaded_type(parser, id),
		c => debug_err!(UnexpectedTokenError {
			file: parser.file,
			activity: ParsingActivity::Type,
			expected: vec![ExpectedValue::Type],
			got: c,
		}),
	}
}

fn parse_offloaded_type(parser: &mut Parser, id: NamespaceId) -> Result<TypeDef, ParseError> {
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
		|value| parse_type(value, id),
		ParsingActivity::OffloadedType,
	)? {
		Some((generics_pos, list)) => (source_pos(parser, name.pos.start, generics_pos.end), list),
		None => (name.pos.clone(), vec![]),
	};

	Ok(TypeDef {
		pos,
		kind: TypeDefKind::Offload(name.data, id),
	})
}

fn source_pos(parser: &Parser, start: TextPos, end: TextPos) -> SourcePos {
	SourcePos {
		file: parser.file,
		start,
		end,
	}
}
