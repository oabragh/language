use thiserror::Error;

#[derive(Error, Debug, PartialEq, Clone)]
pub enum LexicalError {
    #[error("Expected a number after 'e'")]
    ExpectedNumberAfterE,

    #[error("Unclosed string literal")]
    UnclosedStringLiteral,

    #[error("Invalid token '{0}'")]
    InvalidToken(char),

    #[error("Invalid escape sequence '{0}")]
    InvalidEscapeSequence(char),

    #[error("Missing escape sequence")]
    MissingEscapeSequence,

    #[error("Unclosed character literal")]
    UnclosedCharacterLiteral,

    #[error("Character literal can't be empty")]
    EmptyCharacterLiteral,

    #[error("Character literal too long '{0}")]
    LongCharacterLiteral(String),
}
