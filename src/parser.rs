use crate::compilation_manager::CompilationManager;
use crate::keyword::Keyword;
use crate::lexer::{Lexer, LexerError, TextPos, Token, TokenKind};
use crate::operator::OpKind;
use crate::string_pile::TinyString;
use crate::types::{FunctionHeader, Type};

pub struct Parser<'a> {
    pub manager: &'a CompilationManager,
    pub namespace: TinyString,
    pub file: TinyString,
    pub tokens: Lexer<'a>,
}

impl Parser<'_> {
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
}

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
    ExpectedConstantAssignment,
    UnexpectedEndOfFile,
}

pub fn parse_namespace(parser: &mut Parser<'_>) -> Result<(), ParseError> {
    // Since we are currently in a pure namespace, we expect
    // namespace definitions!
    // No AST yet, because this isn't an executable part of the
    // program, just constant definitions!
    while let Some(_) = parser.peek_token(0)? {
        parse_constant_definition(parser)?;
    }

    Ok(())
}

// Doesn't have to return anything, as it adds a compilation unit
// to the CompilationManager anyway
pub fn parse_constant_definition(parser: &mut Parser<'_>) -> Result<(), ParseError> {
    // There HAS to be an identifier here
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
            return Err(ParseError {
                loc: (
                    TextPos {
                        line: 9999999999,
                        character: 9999999999,
                    },
                    TextPos {
                        line: 99999999999,
                        character: 9999999999,
                    },
                ),
                file: parser.file.clone(),
                kind: ParseErrorKind::UnexpectedEndOfFile,
            })
        }
    }

    // We are now sure that this is a namespace assignment,
    // so now we can eat the tokens
    parser.tokens.eat_token();
    parser.tokens.eat_token();

    // Now figure out what kind of constant it is
    match parser.peek_token(0)? {
        Some(Token {
            kind: TokenKind::Keyword(Keyword::Extern),
            start,
            end,
        }) => {
            parser.tokens.eat_token();
        }
        _ => unimplemented!(),
    }

    let mut namespace_entry = parser.namespace.to_string();
    namespace_entry.push('\\');
    namespace_entry.push_str(&name.to_string());
    Ok(())
}
