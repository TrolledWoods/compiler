use crate::compilation_manager::{CompileManager, Dependencies, Identifier};
use crate::error::{CompileError, ErrorPrintingData};
use crate::keyword::Keyword;
use crate::lexer::{BracketKind, Lexer, LexerError, SourcePos, TextPos, Token, TokenKind};
use crate::namespace::{NamespaceError, NamespaceId};
use crate::operator::OpKind;
use crate::string_pile::TinyString;
use crate::types::{FunctionHeader, PrimitiveKind, TypeDef, TypeDefKind};
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
        Ok(self.tokens.eat_token()?)
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
        }
    }
}

#[derive(Debug, Clone)]
pub enum GotAsToken {
    Some(Token),

    /// This means we were at the end of the
    /// file but didn't get a token we had
    /// to get.
    None,
}

impl From<Option<Token>> for GotAsToken {
    fn from(other: Option<Token>) -> GotAsToken {
        match other {
            Some(token) => GotAsToken::Some(token),
            None => GotAsToken::None,
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
                    GotAsToken::Some(token) => SourcePos {
                        file: error.file,
                        start: token.start,
                        end: token.end,
                    },
                    GotAsToken::None => SourcePos {
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
    pub got: GotAsToken,
}

#[derive(Debug)]
pub enum ExpectedValue {
    Identifier,
    FileNamespace,
    Type,
    ConstantDefinitionValue,

    Kind(TokenKind),

    Keyword(Keyword),
    ClosingBracket(BracketKind),
    OpeningBracket(BracketKind),
}

impl Display for ExpectedValue {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use ExpectedValue::*;
        match self {
            Identifier => write!(f, "identifier"),
            FileNamespace => write!(f, "namespaced file"),
            Type => write!(f, "type"),
            ConstantDefinitionValue => write!(f, "constant definition value"),
            Kind(kind) => write!(f, "kind '{:?}'", kind),
            Keyword(keyword) => write!(f, "keyword '{:?}'", keyword),
            ClosingBracket(kind) => write!(f, "closing {:?}", kind),
            OpeningBracket(kind) => write!(f, "opening {:?}", kind),
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

        parse_constant_definition(parser, id)?;
    }

    if !in_block {
        Ok(())
    } else {
        Err(UnexpectedTokenError {
            file: parser.file,
            activity: ParsingActivity::Namespace,
            expected: vec![ExpectedValue::ClosingBracket(BracketKind::Curly)],
            got: GotAsToken::None,
        }
        .into())
    }
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
            got: token.into(),
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
            got: token.into(),
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
            got: token.into(),
        }
        .into()),
    }
}

// Doesn't have to return anything, as it adds a compilation unit
// to the CompilationManager anyway
pub fn parse_constant_definition(
    parser: &mut Parser<'_>,
    namespace_id: NamespaceId,
) -> Result<(), ParseError> {
    let identifier = parse_identifier(parser, ParsingActivity::Constant)?;

    // Expect a constant assignment
    // TODO: Parse an optional type parameter here
    parse_kind(
        parser,
        &TokenKind::Operator {
            kind: OpKind::Constant,
            is_assignment: false,
        },
        ParsingActivity::Constant,
    )?;

    // Now figure out what kind of constant it is
    let token = parser.peek_token(0)?;
    match token {
        // Assume for now that it's a type.
        // That won't be the case forever, though
        c => {
            let mut _deps = Vec::new();
            let type_def = parse_type(parser, &mut _deps)?;
            parser
                .manager
                .insert_named_type(namespace_id, identifier, type_def)?;
        } // _ => {
          //     Err(UnexpectedTokenError {
          //         file: parser.file,
          //         activity: ParsingActivity::ConstantValue,
          //         expected: vec![ExpectedValue::ConstantDefinitionValue],
          //         got: token.into(),
          //     })?;
          // }
    }

    // Even const expressions have to have semicolons.
    // (Mostly for consistancy reasons)
    parse_kind(parser, &TokenKind::Terminator, ParsingActivity::Constant)?;

    Ok(())
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
            got: parser.peek_token(0)?.into(),
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
        let _colon = parse_kind(
            parser,
            &TokenKind::Operator {
                kind: OpKind::Declaration,
                is_assignment: false,
            },
            activity,
        )?;
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

    // pub fn square() -> ListGrammar {
    //     ListGrammar {
    //         start: TokenKind::OpeningBracket(BracketKind::Brack),
    //         separator: TokenKind::Separator,
    //         end: TokenKind::ClosingBracket(BracketKind::Brack),
    //     }
    // }

    pub fn curly() -> ListGrammar {
        ListGrammar {
            start: TokenKind::OpeningBracket(BracketKind::Curly),
            separator: TokenKind::Separator,
            end: TokenKind::ClosingBracket(BracketKind::Curly),
        }
    }

    pub fn angle() -> ListGrammar {
        ListGrammar {
            start: TokenKind::Operator {
                kind: OpKind::Less,
                is_assignment: false,
            },
            separator: TokenKind::Separator,
            end: TokenKind::Operator {
                kind: OpKind::Greater,
                is_assignment: false,
            },
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
            got: parser.peek_token(0)?.into(),
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

fn parse_struct(
    parser: &mut Parser,
    dependencies: &mut Dependencies,
) -> Result<TypeDef, ParseError> {
    parse_keyword(parser, Keyword::Struct, ParsingActivity::Struct)?;

    let (pos, members) = parse_named_list(
        parser,
        ListGrammar::curly(),
        |parser| parse_type(parser, dependencies),
        ParsingActivity::StructMember,
    )?;

    let members: Vec<(TinyString, TypeDef)> = members
        .into_iter()
        .map(|(name, v)| (name.data, v))
        .collect();

    Ok(TypeDef {
        pos,
        kind: TypeDefKind::Struct(members),
    })
}

fn parse_type(parser: &mut Parser, dependencies: &mut Dependencies) -> Result<TypeDef, ParseError> {
    match parser.peek_token(0)? {
        Some(Token {
            kind: TokenKind::Keyword(Keyword::Struct),
            ..
        }) => parse_struct(parser, dependencies),
        Some(Token {
            kind:
                TokenKind::Operator {
                    kind: OpKind::Mul,
                    is_assignment: false,
                },
            start,
            ..
        }) => {
            parser.eat_token()?;
            let internal_type = parse_type(parser, dependencies)?;
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
        }) => parse_offloaded_type(parser, dependencies),
        Some(Token {
            kind: TokenKind::OpeningBracket(BracketKind::Paren),
            ..
        }) => {
            let (pos, tuple) = parse_tuple_type(parser, dependencies)?;

            if let Some(_) = try_parse_kind(
                parser,
                &TokenKind::Operator {
                    kind: OpKind::ReturnArrow,
                    is_assignment: false,
                },
            )? {
                // This is a function pointer!
                let (return_pos, return_tuple) = parse_tuple_type(parser, dependencies)?;

                Ok(TypeDef {
                    pos: SourcePos {
                        file: parser.file,
                        start: pos.start,
                        end: return_pos.end,
                    },
                    kind: TypeDefKind::FunctionPtr(FunctionHeader {
                        args: tuple,
                        returns: return_tuple,
                    }),
                })
            } else {
                // This is just a tuple
                Ok(TypeDef {
                    pos,
                    kind: TypeDefKind::Tuple(tuple),
                })
            }
        }
        c => Err(UnexpectedTokenError {
            file: parser.file,
            activity: ParsingActivity::Type,
            expected: vec![ExpectedValue::Type],
            got: c.into(),
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
        |value| parse_type(value, dependencies),
        ParsingActivity::Tuple,
    )?;

    Ok((pos, members))
}

fn parse_offloaded_type(
    parser: &mut Parser,
    dependencies: &mut Dependencies,
) -> Result<TypeDef, ParseError> {
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
        |value| parse_type(value, dependencies),
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

    let mut added_dep = false;
    for (dep, positions) in dependencies.iter_mut() {
        if *dep == name.data {
            positions.push(name.pos.clone());
            added_dep = true;
            break;
        }
    }
    if !added_dep {
        dependencies.push((name.data, vec![name.pos.clone()]));
    }

    Ok(TypeDef {
        pos,
        kind: TypeDefKind::Offload(name.data),
    })
}
