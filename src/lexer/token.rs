use crate::lexer::error::LexicalError;

pub type Token = Spanned<TokenType>;

#[derive(Debug, PartialEq, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub location: Location,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Location {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    OpAdd,      // `+`
    OpSub,      // `-`
    OpMul,      // `*`
    OpDiv,      // `/`
    OpMod,      // `%`
    OpExp,      // `**`
    OpFloorDiv, // `//`

    OpEqual,      // `=`
    OpAddEq,      // `+=`
    OpSubEq,      // `-=`
    OpMulEq,      // `*=`
    OpDivEq,      // `/=`
    OpModEq,      // `%=`
    OpExpEq,      // `**=`
    OpFloorDivEq, // `//=`
    OpBwAndEq,    // `&=`
    OpBwOrEq,     // `|=`
    OpBwXorEq,    // `^=`
    OpBwNotEq,    // `~=`
    OpBwLShiftEq, // `<<=`
    OpBwRShiftEq, // `>>=`

    OpEqualEq,   // `==`
    OpNotEq,     // `!=`
    OpLess,      // `<`
    OpGreater,   // `>`
    OpLessEq,    // `<=`
    OpGreaterEq, // `>=`

    OpNot, // `!`
    OpAnd, // `&&`
    OpOr,  // `||`

    OpBwAnd,    // `&`
    OpBwOr,     // `|`
    OpBwXor,    // `^`
    OpBwNot,    // `~`
    OpBwLShift, // `<<`
    OpBwRShift, // `>>`

    LParen,   // `(`
    RParen,   // `)`
    LBracket, // `[`
    RBracket, // `]`
    LBrace,   // `{`
    RBrace,   // `}`

    At,          // `@`
    Colon,       // `:`
    ColonColon,  // `::`
    Dot,         // `.`
    Question,    // `?`
    RArrow,      // `->`
    DoubleArrow, // `=>`
    Pipe,        // `|>`
    Semicolon,   // `;`

    KwAssert,   // `assert`
    KwBreak,    // `break`
    KwConst,    // `const`
    KwContinue, // `continue`
    KwDie,      // `die`
    KwElse,     // `else`
    KwFalse,    // `false`
    KwOver,     // `over`
    KwFn,       // `fn`
    KwFunction, // `function`
    KwIf,       // `if`
    KwIn,       // `in`
    KwLoop,     // `loop`
    KwMatch,    // `match`
    KwModule,   // `module`
    KwReturn,   // `return`
    KwTrue,     // `true`
    KwUntil,    // `until`
    KwVar,      // `var`
    KwWhile,    // `while`
    KwImport,   // `import`
    KwPublic,   // `public`

    LtString(String),
    LtChar(char),
    LtInteger(isize),
    LtFloat(f64),

    Identifier(String),

    Comment { content: String, is_doc: bool },
    Error(LexicalError),
}
