use thiserror::Error;

use crate::lexer::token::TokenType;

#[derive(Debug, Error, PartialEq)]
pub enum SyntaxError {
    #[error("Unexpected token")]
    UnexpectedToken(TokenType),

    #[error("EOI")]
    EndOfInput,
}
