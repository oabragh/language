#[cfg(test)]
mod tests;

pub mod error;
pub mod token;

use std::iter::Peekable;
use std::str::Chars;

use error::LexicalError;
use token::{Location, Token, TokenType};

pub struct Lexer<'a> {
    /// The character stream to tokenize, peekable for lookahead.
    pub source: Peekable<Chars<'a>>,
    /// The current position of the lexer.
    index: usize,
    /// A buffer to hold the value of the current token being parsed.
    /// why? im lazy to create a temp variable everywhere
    value: String,
}

impl<'a> From<&'a str> for Lexer<'a> {
    fn from(value: &'a str) -> Self {
        Self {
            source: value.chars().peekable(),
            index: 0,
            value: String::new(),
        }
    }
}

impl<'a> Lexer<'a> {
    fn peek(&mut self) -> Option<&char> {
        self.source.peek()
    }

    fn advance(&mut self) -> Option<char> {
        self.index += 1;
        self.source.next()
    }

    fn process(&mut self, expected: char) -> bool {
        if self.peek() == Some(&expected) {
            self.advance();
            return true;
        }
        false
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        use TokenType::*;

        // Reset the temporal value
        self.value.clear();

        while let Some(next) = self.peek() {
            if !next.is_whitespace() {
                break;
            }

            self.advance();
        }

        let start = self.index;

        let tokentype = match self.advance()? {
            '#' => {
                let is_doc = self.process('!');

                while self.peek() != Some(&'\n') {
                    let next = self.advance().unwrap();

                    self.value.push(next);
                }

                Comment {
                    content: self.value.clone(),
                    is_doc,
                }
            }
            '+' => {
                if self.process('=') {
                    OpAddEq
                } else {
                    OpAdd
                }
            }
            '-' => {
                if self.process('>') {
                    RArrow
                } else if self.process('=') {
                    OpSubEq
                } else {
                    OpSub
                }
            }
            '*' => {
                if self.process('*') {
                    if self.process('=') {
                        OpExpEq
                    } else {
                        OpExp
                    }
                } else if self.process('=') {
                    OpMulEq
                } else {
                    OpMul
                }
            }

            '/' => {
                if self.process('/') {
                    if self.process('=') {
                        OpFloorDivEq
                    } else {
                        OpFloorDiv
                    }
                } else if self.process('=') {
                    OpDivEq
                } else {
                    OpDiv
                }
            }

            '%' => {
                if self.process('=') {
                    OpModEq
                } else {
                    OpMod
                }
            }

            '&' => {
                if self.process('=') {
                    OpBwAndEq
                } else if self.process('&') {
                    OpAnd
                } else {
                    OpBwAnd
                }
            }

            '|' => {
                if self.process('>') {
                    Pipe
                } else if self.process('=') {
                    OpBwOrEq
                } else if self.process('|') {
                    OpOr
                } else {
                    OpBwOr
                }
            }

            '!' => {
                if self.process('=') {
                    OpNotEq
                } else {
                    OpNot
                }
            }

            '^' => {
                if self.process('=') {
                    OpBwXorEq
                } else {
                    OpBwXor
                }
            }

            '~' => {
                if self.process('=') {
                    OpBwNotEq
                } else {
                    OpBwNot
                }
            }

            '=' => {
                if self.process('>') {
                    DoubleArrow
                } else if self.process('=') {
                    OpEqualEq
                } else {
                    OpEqual
                }
            }

            '>' => {
                if self.process('>') {
                    if self.process('=') {
                        OpBwRShiftEq
                    } else {
                        OpBwRShift
                    }
                } else if self.process('=') {
                    OpGreaterEq
                } else {
                    OpGreater
                }
            }

            '<' => {
                if self.process('<') {
                    if self.process('=') {
                        OpBwLShiftEq
                    } else {
                        OpBwLShift
                    }
                } else if self.process('=') {
                    OpLessEq
                } else {
                    OpLess
                }
            }

            '(' => LParen,
            ')' => RParen,
            '[' => LBracket,
            ']' => RBracket,
            '{' => LBrace,
            '}' => RBrace,

            '.' => Dot,
            ':' => {
                if self.process(':') {
                    ColonColon
                } else {
                    Colon
                }
            }
            ';' => Semicolon,
            '?' => Question,
            '@' => At,

            ch @ ('a'..='z' | 'A'..='Z' | '_') => {
                self.value.push(ch);

                while matches!(
                    self.peek(),
                    Some('a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '$')
                ) {
                    let next = self.advance().unwrap();

                    self.value.push(next);
                }

                match self.value.as_str() {
                    "assert" => KwAssert,
                    "break" => KwBreak,
                    "const" => KwConst,
                    "continue" => KwContinue,
                    "die" => KwDie,
                    "over" => KwOver,
                    "else" => KwElse,
                    "false" => KwFalse,
                    "fn" => KwFn,
                    "function" => KwFunction,
                    "if" => KwIf,
                    "in" => KwIn,
                    "loop" => KwLoop,
                    "match" => KwMatch,
                    "module" => KwModule,
                    "return" => KwReturn,
                    "true" => KwTrue,
                    "until" => KwUntil,
                    "var" => KwVar,
                    "while" => KwWhile,
                    "import" => KwImport,
                    "public" => KwPublic,
                    _ => Identifier(self.value.clone()),
                }
            }
            ch @ ('0'..='9') => {
                self.value.push(ch);
                let mut float = false;
                let mut error: Option<LexicalError> = None;

                while matches!(self.peek(), Some('0'..='9')) {
                    let next = self.advance().unwrap();

                    self.value.push(next);
                }

                if let Some('.') = self.peek() {
                    float = true;

                    let dot = self.advance().unwrap();
                    self.value.push(dot);

                    while matches!(self.peek(), Some('0'..='9')) {
                        let next = self.advance().unwrap();

                        self.value.push(next);
                    }
                }

                if let Some('e' | 'E') = self.peek() {
                    float = true;

                    let e = self.advance().unwrap();
                    self.value.push(e);

                    if let Some('+' | '-') = self.peek() {
                        let sign = self.advance().unwrap();
                        self.value.push(sign);
                    }

                    if let Some('0'..='9') = self.peek() {
                        while matches!(self.peek(), Some('0'..='9')) {
                            let next = self.advance().unwrap();

                            self.value.push(next);
                        }
                    } else {
                        error = Some(LexicalError::ExpectedNumberAfterE);
                    }
                }

                if let Some(error) = error {
                    Error(error)
                } else if float {
                    LtFloat(self.value.clone().parse::<f64>().unwrap())
                } else {
                    LtInteger(self.value.clone().parse::<isize>().unwrap())
                }
            }
            '\"' => loop {
                match self.peek() {
                    Some('"') => {
                        self.advance();

                        break LtString(self.value.clone());
                    }
                    Some('\n') | None => break Error(LexicalError::UnclosedStringLiteral),
                    Some(_) => {
                        let next = self.advance().unwrap();

                        if next == '\\' {
                            match self.advance() {
                                Some('n') => self.value.push('\n'),
                                Some('t') => self.value.push('\t'),
                                Some('r') => self.value.push('\r'),
                                Some('\\') => self.value.push('\\'),
                                Some('"') => self.value.push('"'),
                                Some('\'') => self.value.push('\''),
                                Some('0') => self.value.push('\0'),
                                // TODO: handle \x & \u escape sequences
                                Some(ch) => break Error(LexicalError::InvalidEscapeSequence(ch)),
                                _ => break Error(LexicalError::MissingEscapeSequence),
                            }
                        } else {
                            self.value.push(next);
                        }
                    }
                }
            },
            '\'' => loop {
                match self.peek() {
                    Some('\'') => {
                        self.advance();

                        let character = self.value.clone();

                        break match self.value.len() {
                            0 => Error(LexicalError::EmptyCharacterLiteral),
                            1 => LtChar(self.value.chars().next().unwrap()),
                            _ => Error(LexicalError::LongCharacterLiteral(character)),
                        };
                    }
                    Some('\n') | None => break Error(LexicalError::UnclosedCharacterLiteral),
                    Some(_) => {
                        let next = self.advance().unwrap();

                        if next == '\\' {
                            match self.advance() {
                                Some('n') => self.value.push('\n'),
                                Some('t') => self.value.push('\t'),
                                Some('r') => self.value.push('\r'),
                                Some('\\') => self.value.push('\\'),
                                Some('\'') => self.value.push('\''),
                                Some('\"') => self.value.push('\"'),
                                Some('0') => self.value.push('\0'),
                                // TODO: handle \x & \u escape sequences
                                Some(ch) => break Error(LexicalError::InvalidEscapeSequence(ch)),
                                _ => break Error(LexicalError::MissingEscapeSequence),
                            }
                        } else {
                            self.value.push(next);
                        }
                    }
                }
            },
            ch => Error(LexicalError::InvalidToken(ch)),
        };

        let end = self.index;

        Some(Token {
            value: tokentype,
            location: Location { start, end },
        })
    }
}
