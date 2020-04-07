use crate::compilation_manager::{CompileManager, StructDef, ID};
use crate::keyword::Keyword;
use crate::lexer::{BracketKind, Lexer, LexerError, TextPos, Token, TokenKind};
use crate::namespace::{ Publicity, ExportMode, InsertContentError, NamespaceID, NamespaceManager};
use crate::operator::OpKind;
use crate::string_pile::TinyString;

pub struct Parser<'a> {
    pub manager: &'a CompileManager,
    pub file: TinyString,
    pub tokens: Lexer<'a>,
}

impl Parser<'_> {
    fn eat_token(&mut self) -> Result<Option<Token>, ParseError> {
        let token = self.tokens.eat_token();
        match token {
            Ok(t) => Ok(t),
            Err(error) => Err(ParseError {
                loc: (error.pos, error.pos),
                file: self.file,
                kind: ParseErrorKind::LexerError(error),
            }),
        }
    }

    fn peek_token(&mut self, n: usize) -> Result<Option<Token>, ParseError> {
        let token = self.tokens.peek_token(n);
        match token {
            Ok(t) => Ok(t),
            Err(error) => Err(ParseError {
                loc: (error.pos, error.pos),
                file: self.file.clone(),
                kind: ParseErrorKind::LexerError(error),
            }),
        }
    }

    pub fn unexpected_end_of_file(&self) -> ParseError {
        ParseError {
            loc: (TextPos::end_of_file(), TextPos::end_of_file()),
            file: self.file,
            kind: ParseErrorKind::UnexpectedEndOfFile,
        }
    }
}

// TODO: This error type is a bit underengineered for the
// purpose it's serving, I feel like it should have more
// data about what the compiler was trying to do, and what the
// discrepancy between what it was doing and what the code
// was
#[derive(Debug)]
pub struct ParseError {
    pub loc: (TextPos, TextPos),
    pub file: TinyString,
    pub kind: ParseErrorKind,
}

#[derive(Debug)]
pub enum ParseErrorKind {
    LexerError(LexerError),
    ExpectedIdentifier,
    ExpectedTerminator,
    ExpectedConstantAssignment,
    UnexpectedEndOfFile,
    DuplicateNames(InsertContentError<ID>),

    // Struct stuff.
    ExpectedIdentifierInStructMember,
    ExpectedStructMemberSeparator,
    ExpectedDeclarationForStructMember,
    ExpectedOpeningBracketAfterStruct,
    ExpectedTypeInStructMember,
    ExpectedClosingBracket(BracketKind),
}

pub fn parse_namespace(
    parser: &mut Parser<'_>,
    in_block: bool,
    id: NamespaceID,
) -> Result<(), ParseError> {
    // Since we are currently in a pure namespace, we expect
    // namespace definitions!
    // No AST yet, because this isn't an executable part of the
    // program, just constant definitions!
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
        Err(ParseError {
            loc: (parser.tokens.current_pos, parser.tokens.current_pos),
            file: parser.file,
            kind: ParseErrorKind::ExpectedClosingBracket(BracketKind::Curly),
        })
    }
}

// Doesn't have to return anything, as it adds a compilation unit
// to the CompilationManager anyway
pub fn parse_constant_definition(parser: &mut Parser<'_>, namespace_id: NamespaceID) -> Result<(), ParseError> {
    // Is this public?
    let publicity = match parser.peek_token(0)? {
        Some(Token {
            kind: TokenKind::Keyword(Keyword::Public),
            ..
        }) => Publicity::Public,
        _ => Publicity::Private,
    };

    let (name, name_token) = match parser.peek_token(0)? {
        Some(Token {
            kind: TokenKind::Identifier(name),
            start,
            end,
        }) => (name, parser.peek_token(0)?),
        Some(Token { start, end, .. }) => {
            return Err(ParseError {
                loc: (start, end),
                file: parser.file.clone(),
                kind: ParseErrorKind::ExpectedIdentifier,
            })
        }
        // Someone(I) was dumb and called "parse_constant_definition" at the end of a file.
        // This would have given some crazy stupid error message anyways, so we might
        // as well panic
        None => unreachable!(),
    };

    // Expect a constant assignment
    match parser.peek_token(1)? {
        Some(Token {
            kind:
                TokenKind::Operator {
                    kind: OpKind::Constant,
                    is_assignment: false,
                },
            ..
        }) => (),
        Some(Token { start, end, .. }) => {
            return Err(ParseError {
                loc: (start, end),
                file: parser.file.clone(),
                kind: ParseErrorKind::ExpectedConstantAssignment,
            })
        }
        None => {
            return Err(parser.unexpected_end_of_file())
        }
    }

    // We are now sure that this is a namespace assignment,
    // so now we can eat the tokens
    parser.eat_token()?;
    parser.eat_token()?;

    // Now figure out what kind of constant it is
    match parser.peek_token(0)? {
        Some(Token {
            kind: TokenKind::Keyword(Keyword::Struct),
            start,
            end,
        }) => {
            parser.eat_token()?;

            let s = parse_struct(parser)?;
            // parser.manager.insert(parser.namespace,
            //                       if is_public {
            //                           Publicity::Public
            //                       }else {
            //                           Publicity::Private
            //                       },
            //                       name,
            //                       s
            //                       )?;
            println!("Parsed a struct in {}! {:?}", 
                     parser.manager.
                         namespace.get_path(namespace_id),
                     s);
        },
        Some(Token {
            kind: TokenKind::OpeningBracket(BracketKind::Curly),
            start,
            end,
        }) => {
            parser.eat_token()?;

            let sub_namespace = parser
                .manager.namespace
                .create_named_namespace(
                namespace_id,
                name,
                publicity,
                Some(ExportMode::All),
                ).expect("My error handling is really bad atm, soryry");
            parse_namespace(parser, true, sub_namespace)?;
        },
        _ => unimplemented!(),
    }

    // Even const expressions have to have semicolons.
    // (Mostly for consistancy reasons)
    match parser.eat_token()? {
        Some(Token {
            kind: TokenKind::Terminator,
            ..
        }) => (),
        Some(Token { start, end, .. }) => {
            return Err(ParseError {
                loc: (start, end),
                kind: ParseErrorKind::ExpectedTerminator,
                file: parser.file,
            });
        },
        None => {
            return Err(parser.unexpected_end_of_file());
        },
    }

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
    terminator: TokenKind
    ) -> Result<Vec<(TinyString, V)>, ParseError> {

    // Read the members
    let mut members = Vec::new();
    loop {
        // Read member name / terminator
        let name = match parser.eat_token()? {
            Some(Token {
                kind: x,
                ..
            }) if x == terminator => {
                break;
            },
            Some(Token {
                kind: TokenKind::Identifier(name),
                ..
            }) => name,
            Some(Token { start, end, .. }) => {
                return Err(ParseError {
                    loc: (start, end),
                    file: parser.file,
                    kind: ParseErrorKind::ExpectedIdentifierInStructMember,
                });
            },
            None => {
                return Err(parser.unexpected_end_of_file());
            },
        };

        // Read in a ':'
        match parser.eat_token()? {
            Some(Token {
                kind:
                    TokenKind::Operator {
                        kind: OpKind::Declaration,
                        is_assignment: false,
                    },
                ..
            }) => (),
            Some(Token { start, end, .. }) => {
                return Err(ParseError {
                    loc: (start, end),
                    file: parser.file,
                    kind: ParseErrorKind::ExpectedDeclarationForStructMember,
                });
            }
            None => {
                return Err(parser.unexpected_end_of_file());
            }
        }

        // Parse the value
        let value = parse_value(parser)?;

        // We know enough information to do this now
        members.push((name, value));

        // Read a separator or an ending curly bracket
        match parser.eat_token()? {
            Some(Token {
                kind: x,
                ..
            }) if x == terminator => break,
            Some(Token {
                kind: TokenKind::Separator,
                ..
            }) => (),
            Some(Token { start, end, .. }) => {
                return Err(ParseError {
                    loc: (start, end),
                    file: parser.file,
                    kind: ParseErrorKind::ExpectedStructMemberSeparator,
                });
            }
            None => {
                return Err(parser.unexpected_end_of_file());
            }
        };
    }

    Ok(members)
}

fn parse_struct(parser: &mut Parser) -> Result<StructDef, ParseError> {
    // Expect an opening bracket
    match parser.eat_token()? {
        Some(Token {
            kind: TokenKind::OpeningBracket(BracketKind::Curly),
            ..
        }) => (),
        Some(Token { start, end, .. }) => {
            return Err(ParseError {
                loc: (start, end),
                file: parser.file,
                kind: ParseErrorKind::ExpectedOpeningBracketAfterStruct,
            });
        }
        None => {
            return Err(parser.unexpected_end_of_file());
        }
    }

    // Parse the named list of types
    let members = parse_named_list(parser, |parser| { 
        match parser.eat_token()? {
            Some(Token {
                kind: TokenKind::Identifier(name),
                ..
            }) => Ok(name),
            Some(Token { start, end, .. }) => {
                Err(ParseError {
                    loc: (start, end),
                    file: parser.file,
                    kind: ParseErrorKind::ExpectedTypeInStructMember,
                })
            }
            None => {
                return Err(parser.unexpected_end_of_file());
            }
        }
    }, TokenKind::ClosingBracket(BracketKind::Curly))?;

    Ok(StructDef { members })
}
