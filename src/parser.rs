use crate::compilation_manager::{CompileManager, Identifier, StructDef, ID};
use crate::keyword::Keyword;
use crate::lexer::{BracketKind, Lexer, LexerError, TextPos, Token, TokenKind};
use crate::namespace::{ExportMode, InsertContentError, NamespaceID, NamespaceManager, Publicity};
use crate::operator::OpKind;
use crate::string_pile::TinyString;
use crate::SRC_EXTENSION;
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
        let token = self.tokens.eat_token();
        match token {
            Ok(t) => Ok(t),
            Err(error) => Err(ParseError {
                activity: doing,
                file: self.file.clone(),
                expected: vec![],
                got: GotAsToken::Error(error),
            }),
        }
    }

    fn peek_token(
        &mut self,
        doing: ParsingActivity,
        n: usize,
    ) -> Result<Option<Token>, ParseError> {
        let token = self.tokens.peek_token(n);
        match token {
            Ok(t) => Ok(t),
            Err(error) => Err(ParseError {
                activity: doing,
                file: self.file.clone(),
                expected: vec![],
                got: GotAsToken::Error(error),
            }),
        }
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

#[derive(Debug, Clone)]
pub enum GotAsToken {
    Some(Token),
    InvalidState,
    Error(LexerError),

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
pub struct ParseError {
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
    id: NamespaceID,
    mut allow_loads: Option<(PathBuf, &mut Vec<JoinHandle<Vec<ParseError>>>)>,
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

        match &mut allow_loads {
            Some((allow_loads, active_threads)) => {
                parse_constant_definition(parser, id, Some((allow_loads, *active_threads)))?;
            },
            None => {
                parse_constant_definition(parser, id, None);
            }
        }
    }

    if !in_block {
        Ok(())
    } else {
        Err(ParseError {
            file: parser.file,
            activity: ParsingActivity::Namespace,
            expected: vec![ExpectedValue::ClosingBracket(BracketKind::Curly)],
            got: GotAsToken::None,
        })
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
        _ => Err(ParseError {
            file: parser.file,
            activity: doing,
            expected: vec![ExpectedValue::Kind(kind)],
            got: token.into(),
        }),
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
        _ => Err(ParseError {
            file: parser.file,
            activity: doing,
            expected: vec![ExpectedValue::Keyword(keyword)],
            got: token.into(),
        }),
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
            definition: token.unwrap(),
        }),
        _ => Err(ParseError {
            file: parser.file,
            activity: doing,
            expected: vec![ExpectedValue::Identifier],
            got: token.into(),
        }),
    }
}

// Doesn't have to return anything, as it adds a compilation unit
// to the CompilationManager anyway
pub fn parse_constant_definition(
    parser: &mut Parser<'_>,
    namespace_id: NamespaceID,
    can_load_file: Option<(&Path, &mut Vec<JoinHandle<Vec<ParseError>>>)>,
) -> Result<(), ParseError> {
    // Is this public?
    let publicity_token = try_parse_keyword(parser, Keyword::Public, ParsingActivity::Constant)?;
    let publicity = match &publicity_token {
        Some(_) => Publicity::Public,
        None => Publicity::Private,
    };

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

            println!(
                "{}: {}, {:?}",
                parser.file,
                parser.manager.namespace.get_path(namespace_id),
                s
            );
        }
        Some(Token {
            kind: TokenKind::OpeningBracket(BracketKind::Curly),
            start,
            end,
        }) => {
            parser.eat_token(ParsingActivity::Namespace)?;

            let sub_namespace = parser.manager.namespace.create_named_namespace(
                namespace_id,
                identifier.data,
                publicity,
                Some(ExportMode::All),
            );

            let sub_namespace = match sub_namespace {
                Ok(value) => value,
                Err(err) => {
                    println!("{}", identifier.data);
                    unimplemented!();
                    // return Err(
                    // );
                }
            };
            parse_namespace(parser, true, sub_namespace, None)?;
        },
        Some(Token {
            kind: TokenKind::Keyword(Keyword::Load),
            ..
        }) => {
            println!("Found load! {}", identifier.data);
            parser.eat_token(ParsingActivity::LoadNamespace)?;

            if let Some((folder_name, threads)) = can_load_file {
                let manager = parser.manager.clone();
                let mut folder = PathBuf::from(folder_name);
                println!("In folder: {:?}, file: {}", folder, identifier.data);
                
                threads.push(thread::spawn(move || {
                    let name = identifier.data.to_string();
                    folder.push(&name);

                    let mut file = folder.clone();
                    file.set_extension(SRC_EXTENSION);

                    let input = std::fs::read_to_string(&file).unwrap();

                    let mut lexer = Lexer::new(&input);

                    let sub_namespace = manager.namespace.create_named_namespace(
                        namespace_id,
                        identifier.data,
                        publicity,
                        Some(ExportMode::All),
                    );

                    let sub_namespace = match sub_namespace {
                        Ok(value) => value,
                        Err(err) => {
                            unimplemented!();
                        }
                    };

                    let mut parser = Parser {
                        manager: manager.clone(),
                        file: file.to_str().expect("String conversion not possible :<").into(),
                        tokens: lexer,
                    };

                    let mut threads = Vec::new();
                    let mut errors = Vec::new();

                    match parse_namespace(&mut parser, false, sub_namespace, Some((folder, &mut threads))) {
                        Ok(_) => (),
                        Err(err) => errors.push(err),
                    }

                    for thread in threads {
                        errors.append(&mut thread.join().unwrap());
                    }

                    errors
                }));
            } else {
                return Err(ParseError {
                    file: parser.file,
                    activity: ParsingActivity::LoadNamespace,
                    expected: vec![ExpectedValue::FileNamespace],
                    got: GotAsToken::InvalidState,
                });
            }
        },
        _ => {
            return Err(ParseError {
                file: parser.file,
                activity: ParsingActivity::ConstantValue,
                expected: vec![
                    ExpectedValue::OpeningBracket(BracketKind::Paren),
                    ExpectedValue::OpeningBracket(BracketKind::Curly),
                    ExpectedValue::Keyword(Keyword::Struct),
                    ExpectedValue::Identifier,
                ],
                got: token.into(),
            });
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

fn parse_struct(parser: &mut Parser) -> Result<StructDef, ParseError> {
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

    Ok(StructDef { head, members })
}
