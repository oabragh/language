pub mod error;
mod expr;

use crate::lexer::token::{Token, TokenType};
use crate::lexer::Lexer;
use std::iter::Peekable;

use error::SyntaxError;

pub struct Parser<'a> {
    tokens: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            tokens: lexer.peekable(),
        }
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    fn advance(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    fn consume_token(&mut self, expected: TokenType) -> Result<(), SyntaxError> {
        if let Some(token) = self.advance() {
            if token.value == expected {
                Ok(())
            } else {
                Err(SyntaxError::UnexpectedToken(token.value))
            }
        } else {
            Err(SyntaxError::EndOfInput)
        }
    }

    fn match_token(&mut self, token_types: &[TokenType]) -> Option<Token> {
        if let Some(token) = self.peek() {
            if token_types.contains(&token.value) {
                return Some(self.advance().unwrap());
            }
        }
        None
    }
}
