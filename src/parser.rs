use crate::compilation_manager::{CompileManager, DefinedStruct, Id, Identifier};
use crate::error::{CompileError, ErrorPrintingData};
use crate::keyword::Keyword;
use crate::lexer::{BracketKind, Lexer, LexerError, SourcePos, TextPos, Token, TokenKind};
use crate::namespace::{NamespaceError, NamespaceId, NamespaceManager};
use crate::operator::OpKind;
use crate::string_pile::TinyString;
use crate::types::{FunctionHeader, TypeDef, TypeKind};
use crate::SRC_EXTENSION;
use std::fmt::{self, Display, Formatter};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::thread::{self, JoinHandle};

pub struct Parser<'a> {
    pub manager: Arc<CompileManager>,
    pub file: TinyString,
    pub tokens: Lexer<'a>,
}

impl Parser<'_> {
    fn eat_token(&mut self, doing: ParsingActivity) -> Result<Option<Token>, ParseError> {
        Ok(self.tokens.eat_token()?)
    }

    fn peek_token(
        &mut self,
        doing: ParsingActivity,
        n: usize,
    ) -> Result<Option<Token>, ParseError> {
        Ok(self.tokens.peek_token(n)?)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParsingActivity {
    Debug,
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
            Debug => write!(f, "COMPILER DEBBUGING THING"),
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
            ParseError::Namespace(error) => unimplemented!(),
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
            Kind(kind) => write!(f, "kind '{:?}'", kind),
            Keyword(keyword) => write!(f, "keyword '{:?}'", keyword),
            ClosingBracket(kind) => write!(f, "closing {:?}", kind),
            OpeningBracket(kind) => write!(f, "opening {:?}", kind),
        }
    }
}

/// Just parses a namespace.
/// Namespaces ONLY contain
/// constants, and the order
/// of the constants should never matter,
/// not even which file they are in
/// should matter(other than what namespace
/// they reside in).
/// No opening bracket is expected, but if the
/// `in_block` parameter is true, we expect a
/// closing bracket.
pub fn parse_namespace(
    parser: &mut Parser<'_>,
    in_block: bool,
    id: NamespaceId,
) -> Result<(), ParseError> {
    while let Some(token) = parser.peek_token(ParsingActivity::Namespace, 0)? {
        if let Token {
            kind: TokenKind::ClosingBracket(BracketKind::Curly),
            ..
        } = token
        {
            if in_block {
                parser.eat_token(ParsingActivity::Namespace)?;
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
    let token = parser.eat_token(doing)?;
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
    let token = parser.eat_token(doing)?;
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

fn try_parse_kind(
    parser: &mut Parser<'_>,
    kind: &TokenKind,
    doing: ParsingActivity,
) -> Result<Option<Token>, ParseError> {
    let peeked_token = parser.peek_token(doing, 0)?;
    match &peeked_token {
        Some(Token { kind: kind_, .. }) if kind_ == kind => {
            parser.eat_token(doing)?;
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
    doing: ParsingActivity,
) -> Result<Option<Token>, ParseError> {
    let peeked_token = parser.peek_token(doing, 0)?;
    match peeked_token {
        Some(Token {
            kind: TokenKind::Keyword(k),
            ..
        }) if k == keyword => {
            parser.eat_token(doing)?;
            Ok(peeked_token)
        }
        _ => Ok(None),
    }
}

pub fn parse_identifier(
    parser: &mut Parser<'_>,
    doing: ParsingActivity,
) -> Result<Identifier, ParseError> {
    let token = parser.eat_token(doing)?;
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
    let assign_op = parse_kind(
        parser,
        &TokenKind::Operator {
            kind: OpKind::Constant,
            is_assignment: false,
        },
        ParsingActivity::Constant,
    )?;

    // Now figure out what kind of constant it is
    let token = parser.peek_token(ParsingActivity::ConstantValue, 0)?;
    match token {
        Some(Token {
            kind: TokenKind::Keyword(Keyword::Struct),
            start,
            end,
        }) => {
            let s = parse_struct(parser)?;

            println!("{}", &s);
            parser.manager.insert_struct(namespace_id, identifier, s)?;
        }
        _ => {
            Err(UnexpectedTokenError {
                file: parser.file,
                activity: ParsingActivity::ConstantValue,
                expected: vec![
                    ExpectedValue::OpeningBracket(BracketKind::Paren),
                    ExpectedValue::OpeningBracket(BracketKind::Curly),
                    ExpectedValue::Keyword(Keyword::Struct),
                    ExpectedValue::Identifier,
                ],
                got: token.into(),
            })?;
        }
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
    let start = if let Some(start) = try_parse_kind(parser, &grammar.start, activity)? {
        start.start
    } else {
        return Err(UnexpectedTokenError {
            file: parser.file,
            activity,
            expected: vec![ExpectedValue::Kind(grammar.start)],
            got: parser.peek_token(activity, 0)?.into(),
        }
        .into());
    };

    // Read the members
    let mut members = Vec::new();
    loop {
        if let Some(terminator) = try_parse_kind(parser, &grammar.end, activity)? {
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

        if let Some(terminator) = try_parse_kind(parser, &grammar.end, activity)? {
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
    if let Some(Token { kind, .. }) = parser.peek_token(activity, 0)? {
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
    let start = if let Some(start) = try_parse_kind(parser, &grammar.start, activity)? {
        start.start
    } else {
        return Err(UnexpectedTokenError {
            file: parser.file,
            activity,
            expected: vec![ExpectedValue::Kind(grammar.start)],
            got: parser.peek_token(activity, 0)?.into(),
        }
        .into());
    };

    let mut members = Vec::new();
    loop {
        if let Some(terminator) = try_parse_kind(parser, &grammar.end, activity)? {
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

        if let Some(terminator) = try_parse_kind(parser, &grammar.end, activity)? {
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

fn parse_struct(parser: &mut Parser) -> Result<DefinedStruct, ParseError> {
    // Used for error messages
    let head = parse_keyword(parser, Keyword::Struct, ParsingActivity::Struct)?;

    // Parse the named list of types
    let (_, members) = parse_named_list(
        parser,
        ListGrammar::curly(),
        |parser| parse_type(parser),
        ParsingActivity::StructMember,
    )?;

    Ok(DefinedStruct { head, members })
}

fn parse_type(parser: &mut Parser) -> Result<TypeDef, ParseError> {
    match parser.peek_token(ParsingActivity::Type, 0)? {
        Some(Token {
            kind: TokenKind::Identifier(data),
            ..
        }) => parse_offloaded_type(parser),
        Some(Token {
            kind: TokenKind::OpeningBracket(BracketKind::Paren),
            ..
        }) => {
            let (pos, tuple) = parse_tuple_type(parser)?;

            if let Some(_) = try_parse_kind(
                parser,
                &TokenKind::Operator {
                    kind: OpKind::ReturnArrow,
                    is_assignment: false,
                },
                ParsingActivity::FunctionPtr,
            )? {
                // This is a function pointer!
                let (return_pos, return_tuple) = parse_tuple_type(parser)?;

                Ok(TypeDef {
                    pos: SourcePos {
                        file: parser.file,
                        start: pos.start,
                        end: return_pos.end,
                    },
                    kind: TypeKind::FunctionPtr(FunctionHeader {
                        args: tuple,
                        returns: return_tuple,
                    }),
                })
            } else {
                // This is just a tuple
                Ok(TypeDef {
                    pos,
                    kind: TypeKind::Tuple(tuple),
                })
            }
        }
        c => Err(UnexpectedTokenError {
            file: parser.file,
            activity: ParsingActivity::Type,
            expected: vec![
                ExpectedValue::Identifier,
                ExpectedValue::OpeningBracket(BracketKind::Paren),
            ],
            got: c.into(),
        }
        .into()),
    }
}

fn parse_tuple_type(parser: &mut Parser) -> Result<(SourcePos, Vec<TypeDef>), ParseError> {
    let (pos, members) = parse_list(
        parser,
        ListGrammar::parenthesis(),
        parse_type,
        ParsingActivity::Tuple,
    )?;

    Ok((pos, members))
}

fn parse_offloaded_type(parser: &mut Parser) -> Result<TypeDef, ParseError> {
    let name = parse_identifier(parser, ParsingActivity::OffloadedType)?;

    let (pos, generics) = match try_parse_list(
        parser,
        ListGrammar::angle(),
        parse_type,
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
        kind: TypeKind::Offload { name, generics },
    })
}
