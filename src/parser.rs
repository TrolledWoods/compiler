use crate::compilation_manager::{CompileManager, Identifier, DefinedStruct, Id};
use crate::keyword::Keyword;
use crate::lexer::{SourcePos, BracketKind, Lexer, LexerError, TextPos, Token, TokenKind};
use crate::namespace::{NamespaceError, NamespaceId, NamespaceManager};
use crate::operator::OpKind;
use crate::string_pile::TinyString;
use crate::SRC_EXTENSION;
use crate::error::{CompileError, ErrorPrintingData};
use std::fmt::{self, Formatter, Display};
use std::thread::{self, JoinHandle};
use std::path::{Path, PathBuf};
use std::sync::Arc;

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
                        end: token.end
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
                    }else {
                        format!("{}", value)
                    };

                    expected.push_str(&addition);
                }

                ErrorPrintingData::new(message)
                    .problem(pos, expected)
            },
            ParseError::Lexer(error) => {
                error.get_printing_data()
            },
            ParseError::Namespace(error) => {
                unimplemented!()
            },
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
        }.into())
    }
}

fn parse_kind(
    parser: &mut Parser<'_>,
    kind: TokenKind,
    doing: ParsingActivity,
) -> Result<Token, ParseError> {
    let token = parser.eat_token(doing)?;
    match &token {
        Some(Token { kind: kind_, .. }) if *kind_ == kind => Ok(token.unwrap()),
        _ => Err(UnexpectedTokenError {
            file: parser.file,
            activity: doing,
            expected: vec![ExpectedValue::Kind(kind)],
            got: token.into(),
        }.into()),
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
        }.into()),
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
        }.into()),
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
        TokenKind::Operator {
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

            println!("{:?}", &s);
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
    parse_kind(parser, TokenKind::Terminator, ParsingActivity::Constant)?;

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
    mut parse_value: impl FnMut(&mut Parser) -> Result<V, ParseError>,
    terminator: TokenKind,
    activity: ParsingActivity,
) -> Result<Vec<(Identifier, V)>, ParseError> {
    // Read the members
    let mut members = Vec::new();
    loop {
        // Read member name / terminator
        if let Some(terminator) = try_parse_kind(parser, &terminator, activity)? {
            break;
        }
        let name = parse_identifier(parser, activity)?;

        // Read in a ':'
        let _colon = parse_kind(
            parser,
            TokenKind::Operator {
                kind: OpKind::Declaration,
                is_assignment: false,
            },
            activity,
        )?;

        // Parse the value
        let value = parse_value(parser)?;

        // We know enough information to do this now
        members.push((name, value));

        // Read a separator or the terminator
        if let Some(terminator) = try_parse_kind(parser, &terminator, activity)? {
            break;
        }

        // If we didn't recieve the terminator, we HAVE
        // to get a separator
        parse_kind(parser, TokenKind::Separator, activity)?;
    }

    Ok(members)
}

fn parse_struct(parser: &mut Parser) -> Result<DefinedStruct, ParseError> {
    // Used for error messages
    let head = parse_keyword(parser, Keyword::Struct, ParsingActivity::Struct)?;

    parse_kind(
        parser,
        TokenKind::OpeningBracket(BracketKind::Curly),
        ParsingActivity::Struct,
    )?;

    // Parse the named list of types
    let members = parse_named_list(
        parser,
        |parser| parse_identifier(parser, ParsingActivity::StructMember),
        TokenKind::ClosingBracket(BracketKind::Curly),
        ParsingActivity::StructMember,
    )?;

    Ok(DefinedStruct { head, members })
}
