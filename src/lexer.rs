use crate::keyword::Keyword;
use crate::operator::OpKind;
use crate::string_pile::TinyString;
use crate::error::{CompileError, ErrorPrintingData};
use std::collections::VecDeque;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub start: TextPos,
    pub end: TextPos,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    OpeningBracket(BracketKind),
    ClosingBracket(BracketKind),
    Terminator,
    Separator,
    Identifier(TinyString),
    Operator { kind: OpKind, is_assignment: bool },
    Keyword(Keyword),
    StringLiteral(TinyString),
    IntLiteral(i128), // Store literals with the highest precision, as we can always scale down but not scale up later
    FloatLiteral(f64),
}

#[derive(Clone, Debug)]
pub struct LexerError {
    pub file: TinyString,
    pub kind: LexerErrorKind,
    pub pos: TextPos,
}

impl CompileError for LexerError {
    fn get_printing_data(self) -> ErrorPrintingData {
        use LexerErrorKind::*;
        match self.kind {
            UnexpectedEndOfFile => 
                ErrorPrintingData::new(
                    format!("Unexpected end of file")
                ).problem(SourcePos {
                    file: self.file,
                    start: self.pos,
                    end: self.pos,
                }, 
                format!("Ended here")
                ),
            InvalidToken =>
                ErrorPrintingData::new(
                    format!("Invalid token"),
                ).problem(SourcePos {
                    file: self.file,
                    start: self.pos,
                    end: self.pos,
                },
                format!("Here"),
                ),
            UnclosedStringLiteral { pos } =>
                ErrorPrintingData::new(
                    format!("String literal isn't closed")
                ).problem(SourcePos {
                    file: self.file,
                    start: self.pos,
                    end: pos,
                },
                format!("String that isn't closed")
                ),
            _ => {
                println!("{:?}", self);
                unimplemented!()
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum LexerErrorKind {
    UnexpectedEndOfFile,
    InvalidToken,
    UnclosedStringLiteral { pos: TextPos },
    UnfinishedEscapeCode,
    BadEscapeCode,
    ExpectedChar,
    ManyDecimalPoints,
    InvalidHexU32 { end: TextPos },
    InvalidUnicode(u32),
    InvalidHexDigit(char),
}


#[derive(Clone, Debug, PartialEq)]
pub struct SourcePos {
    pub file: TinyString,
    pub start: TextPos,
    pub end: TextPos,
}

impl SourcePos {
    pub fn from_token(token: &Token, file: TinyString) -> SourcePos {
        SourcePos {
            file,
            start: token.start,
            end: token.end,
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub struct TextPos {
    pub line: usize,
    pub character: usize,
}

impl TextPos {
    pub fn new(line: usize, character: usize) -> TextPos {
        TextPos { line, character }
    }

    pub fn end_of_file() -> TextPos {
        TextPos {
            line: std::usize::MAX,
            character: std::usize::MAX,
        }
    }
}

impl std::fmt::Display for TextPos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.character + 1)
    }
}

impl std::fmt::Debug for TextPos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.line == std::usize::MAX {
            return write!(f, "end of file");
        }

        write!(f, "{}:{}", self.line + 1, self.character + 1)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BracketKind {
    Paren,
    Curly,
    Brack,
    // Potentially <> brackets too?
}

enum ReadTokenState {
    Token(Token),
    SemanticToken, // i.e. a comment of some kind
    EndOfFile,
}

pub struct Lexer<'a> {
    source: Chars<'a>,
    file: TinyString,
    pub current_pos: TextPos,

    peeked_tokens: VecDeque<Token>,
    poisoned: bool,
}

impl Lexer<'_> {
    pub fn new<'a>(file: TinyString, source: &'a str) -> Lexer<'a> {
        Lexer {
            source: source.chars(),
            file,
            current_pos: TextPos {
                line: 0,
                character: 0,
            },

            peeked_tokens: VecDeque::new(),
            poisoned: false,
        }
    }

    pub fn peek_token(&mut self, n_tokens_forward: usize) -> Result<Option<Token>, LexerError> {
        if self.poisoned {
            panic!("Cannot access a lexer after it has thrown an error");
        }

        // See if we already had the token cached
        if n_tokens_forward < self.peeked_tokens.len() {
            return Ok(Some(self.peeked_tokens[n_tokens_forward].clone()));
        } else {
            // Read in more tokens
            let mut n_peeked_tokens = self.peeked_tokens.len();
            let n_wanted_tokens = n_tokens_forward + 1;
            loop {
                match self.read_next_token() {
                    Ok(ReadTokenState::Token(token)) => {
                        self.peeked_tokens.push_back(token);
                        n_peeked_tokens += 1;

                        if n_peeked_tokens == n_wanted_tokens {
                            return Ok(Some(self.peeked_tokens[n_tokens_forward].clone()));
                        }
                    }
                    Ok(ReadTokenState::SemanticToken) => (),
                    Ok(ReadTokenState::EndOfFile) => {
                        return Ok(None);
                    }
                    Err(error) => {
                        self.poisoned = true;
                        return Err(error);
                    }
                }
            }
        }
    }

    /// Tries to eat a token.
    pub fn eat_token(&mut self) -> Result<Option<Token>, LexerError> {
        self.peek_token(0)?;
        Ok(self.peeked_tokens.pop_front())
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.source.next()?;
        if c == '\n' {
            self.current_pos.line += 1;
            self.current_pos.character = 0;
        } else {
            self.current_pos.character += 1;
        }
        Some(c)
    }

    fn peek_char(&mut self) -> Option<char> {
        // The clone should be cheap, right?
        // Two pointers we clone, i.e. 128 bits max?
        // **scared**
        self.source.clone().next()
    }

    fn read_possibly_escaped_char(&mut self) -> Result<char, LexerError> {
        let start = self.current_pos;
        match self.next_char() {
            Some('\\') => {
                // Escaped character!
                match self.next_char() {
                    Some('"') => Ok('"'),
                    Some('\\') => Ok('\\'),
                    Some('n') => Ok('\n'),
                    Some('t') => Ok('\t'),
                    Some('u') => {
                        // Unicode character
                        // Read a hexa decimal number
                        // Since these characters are u32:s,
                        // we can maximally have 8 hexadecimal
                        // digits
                        let number_start = self.current_pos;
                        let mut unicode_number: u32 = 0;
                        for i in 0..9 {
                            match self.next_char() {
                                Some('\\') => break,
                                Some(c) => match c.to_digit(16) {
                                    Some(digit) => {
                                        // Do a check so that the number cannot be too big
                                        if i >= 9 {
                                            return Err(LexerError {
                                                file: self.file,
                                                kind: LexerErrorKind::InvalidHexU32 {
                                                    end: self.current_pos,
                                                },
                                                pos: start,
                                            });
                                        }

                                        unicode_number = (unicode_number << 4) + digit;
                                    }
                                    None => {
                                        return Err(LexerError {
                                            file: self.file,
                                            kind: LexerErrorKind::InvalidHexDigit(c),
                                            pos: self.current_pos,
                                        })
                                    }
                                },
                                None => {
                                    return Err(LexerError {
                                        file: self.file,
                                        kind: LexerErrorKind::UnfinishedEscapeCode,
                                        pos: self.current_pos,
                                    })
                                }
                            }
                        }

                        match std::char::from_u32(unicode_number) {
                            Some(c) => Ok(c),
                            None => {
                                return Err(LexerError {
                                    file: self.file,
                                    kind: LexerErrorKind::InvalidUnicode(unicode_number),
                                    pos: number_start,
                                })
                            }
                        }
                    }
                    Some(_) => {
                        return Err(LexerError {
                            file: self.file,
                            kind: LexerErrorKind::BadEscapeCode,
                            pos: start,
                        })
                    }
                    None => {
                        return Err(LexerError {
                            file: self.file,
                            kind: LexerErrorKind::UnfinishedEscapeCode,
                            pos: self.current_pos,
                        })
                    }
                }
            }
            Some(c) => Ok(c),
            None => {
                return Err(LexerError {
                    file: self.file,
                    kind: LexerErrorKind::ExpectedChar,
                    pos: start,
                })
            }
        }
    }

    fn match_next<'a, T: 'a>(
        &mut self,
        mapper: impl Iterator<Item = &'a (&'static str, T)>,
    ) -> Option<&'a T> {
        for (pattern, item) in mapper {
            if self.is_this_next(pattern) {
                // Read in the characters we matched on
                for _ in 0..pattern.len() {
                    self.next_char();
                }
                return Some(item);
            }
        }

        None
    }

    fn is_this_next(&self, next: &str) -> bool {
        let own = self.source.as_str();
        if own.len() >= next.len() {
            &own[0..next.len()] == next
        } else {
            false
        }
    }

    /// Reads the next token into the queue. Please note
    /// that this function doesn't check if an error has been found
    /// before, and it doesn't set the error value if an error is found.
    /// This is for the caller to deal with
    fn read_next_token(&mut self) -> Result<ReadTokenState, LexerError> {
        while let Some(c) = self.peek_char() {
            if c.is_whitespace() {
                self.next_char();
            } else {
                break;
            }
        }

        match self.peek_char() {
            Some('/') if self.is_this_next("//") => {
                // It's a comment, so read until the end of the line
                while let Some(c) = self.next_char() {
                    if c == '\n' {
                        break;
                    }
                }

                // A semantic token is just a comment for now.
                // It's here to reduce recursion(which would be terrible for the stack,
                // especially if you have like 500 lines of documentation code)
                Ok(ReadTokenState::SemanticToken)
            }
            Some(';') => {
                let start = self.current_pos;
                self.next_char();
                Ok(ReadTokenState::Token(Token {
                    kind: TokenKind::Terminator,
                    start,
                    end: start,
                }))
            }
            Some(',') => {
                let start = self.current_pos;
                self.next_char();
                Ok(ReadTokenState::Token(Token {
                    kind: TokenKind::Separator,
                    start,
                    end: start,
                }))
            }
            Some('(') => {
                let start = self.current_pos;
                self.next_char();
                Ok(ReadTokenState::Token(Token {
                    kind: TokenKind::OpeningBracket(BracketKind::Paren),
                    start,
                    end: start,
                }))
            }
            Some('[') => {
                let start = self.current_pos;
                self.next_char();
                Ok(ReadTokenState::Token(Token {
                    kind: TokenKind::OpeningBracket(BracketKind::Brack),
                    start,
                    end: start,
                }))
            }
            Some('{') => {
                let start = self.current_pos;
                self.next_char();
                Ok(ReadTokenState::Token(Token {
                    kind: TokenKind::OpeningBracket(BracketKind::Curly),
                    start,
                    end: start,
                }))
            }
            Some(')') => {
                let start = self.current_pos;
                self.next_char();
                Ok(ReadTokenState::Token(Token {
                    kind: TokenKind::ClosingBracket(BracketKind::Paren),
                    start,
                    end: start,
                }))
            }
            Some(']') => {
                let start = self.current_pos;
                self.next_char();
                Ok(ReadTokenState::Token(Token {
                    kind: TokenKind::ClosingBracket(BracketKind::Brack),
                    start,
                    end: start,
                }))
            }
            Some('}') => {
                let start = self.current_pos;
                self.next_char();
                Ok(ReadTokenState::Token(Token {
                    kind: TokenKind::ClosingBracket(BracketKind::Curly),
                    start,
                    end: start,
                }))
            }
            Some(c) if c.is_digit(10) || c == '.' => {
                let start = self.current_pos;
                let mut num: i128 = 0;
                let mut decimal_point: Option<u64> = None;

                while let Some(c) = self.peek_char() {
                    if let Some(digit) = c.to_digit(10) {
                        num *= 10;
                        num += digit as i128;

                        // If we are to the right of the decimal point,
                        // increase precision
                        decimal_point = decimal_point.map(|v| v + 1);
                    } else if c == '.' {
                        if decimal_point.is_none() {
                            decimal_point = Some(0);
                        } else {
                            return Err(LexerError {
                                file: self.file,
                                kind: LexerErrorKind::ManyDecimalPoints,
                                pos: start,
                            });
                        }
                    } else {
                        break;
                    }

                    self.next_char();
                }

                if let Some(decimal_point) = decimal_point {
                    Ok(ReadTokenState::Token(Token {
                        kind: TokenKind::FloatLiteral(
                            num as f64 * (0.1f64).powf(decimal_point as f64),
                        ),
                        start: start,
                        end: self.current_pos,
                    }))
                } else {
                    Ok(ReadTokenState::Token(Token {
                        kind: TokenKind::IntLiteral(num),
                        start: start,
                        end: self.current_pos,
                    }))
                }
            }
            Some('+') | Some('-') | Some('/') | Some('*') | Some('%') | Some('^') | Some('&')
            | Some('=') | Some('!') | Some('|') | Some('<') | Some('>') | Some('~') | Some(':') => {
                // Operator!
                let start = self.current_pos;

                // A map from strings to operators where
                // the operator cannot be an assignment operator
                // (as that would be ambiguous)
                const NON_ASSIGN_OP_MAP: &[(&str, OpKind)] = &[
                    ("==", OpKind::Equal),
                    ("!=", OpKind::NotEqual),
                    ("<=", OpKind::LessEq),
                    (">=", OpKind::GreaterEq),
                    ("->", OpKind::ReturnArrow),
                    ("<", OpKind::Less),
                    (">", OpKind::Greater),
                    ("&&", OpKind::And),
                    ("||", OpKind::Or),
                    ("!", OpKind::Not),
                    ("=", OpKind::Assignment),
                    ("::", OpKind::Constant),
                ];

                const OP_MAP: &[(&str, OpKind)] = &[
                    ("+", OpKind::Add),
                    ("-", OpKind::Sub),
                    ("*", OpKind::Mul),
                    ("/", OpKind::Div),
                    ("%", OpKind::Mod),
                    ("^", OpKind::Pow),
                    ("~", OpKind::BitNot),
                    ("&", OpKind::BitAnd),
                    ("|", OpKind::BitOr),
                    (":", OpKind::Declaration),
                ];

                if let Some(op) = self.match_next(NON_ASSIGN_OP_MAP.iter()) {
                    Ok(ReadTokenState::Token(Token {
                        kind: TokenKind::Operator {
                            kind: *op,
                            is_assignment: false,
                        },
                        start: start,
                        end: self.current_pos,
                    }))
                } else if let Some(op) = self.match_next(OP_MAP.iter()) {
                    // See if it's an assignment
                    let is_assignment = self.peek_char() == Some('=');

                    // If there was an equal sign, that was part of the operator
                    if is_assignment {
                        self.next_char();
                    }
                    Ok(ReadTokenState::Token(Token {
                        kind: TokenKind::Operator {
                            kind: *op,
                            is_assignment,
                        },
                        start: start,
                        end: self.current_pos,
                    }))
                } else {
                    // Since all the states that the branch would match have to fit
                    // in one of the above conditions, this shouldn't happen, ever
                    unreachable!();
                }
            }
            Some('"') => {
                let start = self.current_pos;
                self.next_char();

                // Read the string! (i.e. push characters onto it until we find an end)
                let mut string = String::new();
                while self.peek_char() != Some('"') {
                    // "read_possible_escaped_char" will error if the file
                    // ends
                    if self.peek_char() == None {
                        return Err(LexerError {
                            file: self.file,
                            kind: LexerErrorKind::UnclosedStringLiteral { 
                                pos: self.current_pos
                            },
                            pos: start
                        });
                    }
                    string.push(self.read_possibly_escaped_char()?);
                }

                // The final " has to be skipped
                self.next_char();

                Ok(ReadTokenState::Token(Token {
                    kind: TokenKind::StringLiteral(string.into()),
                    start: start,
                    end: self.current_pos,
                }))
            }
            // Identifiers / Keywords
            Some(c) if c.is_alphabetic() => {
                // Find the start location
                let start = self.current_pos;
                self.next_char();

                // Collect every character in the word
                let mut word = String::new();
                word.push(c);
                while let Some(c) = self.peek_char() {
                    if c.is_alphabetic() || c.is_digit(10) || c == '_' {
                        word.push(c);
                        self.next_char();
                    } else {
                        break;
                    }
                }

                Ok(ReadTokenState::Token(Token {
                    kind: match word.as_ref() {
                        "while" => TokenKind::Keyword(Keyword::While),
                        "loop" => TokenKind::Keyword(Keyword::Loop),
                        "for" => TokenKind::Keyword(Keyword::For),
                        "if" => TokenKind::Keyword(Keyword::If),
                        "else" => TokenKind::Keyword(Keyword::Else),
                        "return" => TokenKind::Keyword(Keyword::Return),
                        "struct" => TokenKind::Keyword(Keyword::Struct),
                        "union" => TokenKind::Keyword(Keyword::Union),
                        "type" => TokenKind::Keyword(Keyword::Type),
                        "enum" => TokenKind::Keyword(Keyword::Enum),
                        "extern" => TokenKind::Keyword(Keyword::Extern),
                        "use" => TokenKind::Keyword(Keyword::Use),
                        "module" => TokenKind::Keyword(Keyword::Module),
                        "pub" => TokenKind::Keyword(Keyword::Public),
                        "load" => TokenKind::Keyword(Keyword::Load),
                        _ => TokenKind::Identifier(word.into()),
                    },
                    start: start,
                    end: self.current_pos,
                }))
            }
            None => Ok(ReadTokenState::EndOfFile),
            _ => Err(LexerError {
                file: self.file,
                kind: LexerErrorKind::InvalidToken,
                pos: self.current_pos,
            }),
        }
    }
}
