use super::{TokenType::*, *};

#[test]
fn lex_arithmetic_op() {
    let sample = r#"+ - * / % ** //"#;

    let tokens = Lexer::from(sample)
        .map(|t| t.value)
        .collect::<Vec<TokenType>>();

    assert_eq!(
        tokens,
        vec![OpAdd, OpSub, OpMul, OpDiv, OpMod, OpExp, OpFloorDiv]
    )
}

#[test]
fn lex_assignment_op() {
    let sample = r#"= += -= *= /= %= **= //= &= |= ^= ~= <<= >>="#;

    let tokens = Lexer::from(sample)
        .map(|t| t.value)
        .collect::<Vec<TokenType>>();

    assert_eq!(
        tokens,
        vec![
            OpEqual,
            OpAddEq,
            OpSubEq,
            OpMulEq,
            OpDivEq,
            OpModEq,
            OpExpEq,
            OpFloorDivEq,
            OpBwAndEq,
            OpBwOrEq,
            OpBwXorEq,
            OpBwNotEq,
            OpBwLShiftEq,
            OpBwRShiftEq,
        ]
    )
}

#[test]
fn lex_comparison_op() {
    let sample = r#"== != < > <= >="#;

    let tokens = Lexer::from(sample)
        .map(|t| t.value)
        .collect::<Vec<TokenType>>();

    assert_eq!(
        tokens,
        vec![OpEqualEq, OpNotEq, OpLess, OpGreater, OpLessEq, OpGreaterEq,]
    )
}

#[test]
fn lex_logical_op() {
    let sample = r#"&& || !"#;

    let tokens = Lexer::from(sample)
        .map(|t| t.value)
        .collect::<Vec<TokenType>>();

    assert_eq!(tokens, vec![OpAnd, OpOr, OpNot])
}

#[test]
fn lex_bitwise_op() {
    let sample = r#"& | ^ ~ << >>"#;

    let tokens = Lexer::from(sample)
        .map(|t| t.value)
        .collect::<Vec<TokenType>>();

    assert_eq!(
        tokens,
        vec![OpBwAnd, OpBwOr, OpBwXor, OpBwNot, OpBwLShift, OpBwRShift]
    )
}

#[test]
fn lex_groupings() {
    let sample = r#"( ) [ ] { }"#;

    let tokens = Lexer::from(sample)
        .map(|t| t.value)
        .collect::<Vec<TokenType>>();

    assert_eq!(
        tokens,
        vec![LParen, RParen, LBracket, RBracket, LBrace, RBrace,]
    )
}

#[test]
fn lex_punctuations() {
    let sample = r#"
    @
    :
    ::
    .
    ?
    ->
    =>
    |>
    ;
"#;

    let tokens = Lexer::from(sample)
        .map(|t| t.value)
        .collect::<Vec<TokenType>>();

    assert_eq!(
        tokens,
        vec![
            At,
            Colon,
            ColonColon,
            Dot,
            Question,
            RArrow,
            DoubleArrow,
            Pipe,
            Semicolon
        ]
    )
}

#[test]
fn lex_keywords() {
    let sample = r#"
        assert break const continue
        die else false over fn function
        if in loop match module return
        true until var while import public
    "#;

    let tokens = Lexer::from(sample)
        .map(|t| t.value)
        .collect::<Vec<TokenType>>();

    assert_eq!(
        tokens,
        vec![
            KwAssert, KwBreak, KwConst, KwContinue, KwDie, KwElse, KwFalse, KwOver, KwFn,
            KwFunction, KwIf, KwIn, KwLoop, KwMatch, KwModule, KwReturn, KwTrue, KwUntil, KwVar,
            KwWhile, KwImport, KwPublic
        ]
    )
}

#[test]
fn lex_comments() {
    let sample = r#"
        # Comment~
        #! A doc comment!
    "#;

    let tokens = Lexer::from(sample)
        .map(|t| t.value)
        .collect::<Vec<TokenType>>();

    assert_eq!(
        tokens,
        vec![
            Comment {
                content: " Comment~".to_string(),
                is_doc: false
            },
            Comment {
                content: " A doc comment!".to_string(),
                is_doc: true
            }
        ]
    )
}

#[test]
fn lex_numbers() {
    let sample = r#"
        123
        12.3e1
        1230e-1
        123.0
        123.
        12.e1
        123e
    "#;

    let tokens = Lexer::from(sample)
        .map(|t| t.value)
        .collect::<Vec<TokenType>>();

    assert_eq!(
        tokens,
        vec![
            LtInteger(123),
            LtFloat(123.0),
            LtFloat(123.0),
            LtFloat(123.0),
            LtFloat(123.0),
            LtFloat(120.0),
            Error(LexicalError::ExpectedNumberAfterE)
        ]
    )
}

#[test]
fn lex_string() {
    let sample = r#"
abc
123
"hi"
69
"one\ntwo"
"djdkldjsfj
    "#;

    let tokens = Lexer::from(sample)
        .map(|t| t.value)
        .collect::<Vec<TokenType>>();

    assert_eq!(
        tokens,
        vec![
            Identifier("abc".to_string()),
            LtInteger(123),
            LtString("hi".to_string()),
            LtInteger(69),
            LtString("one\ntwo".to_string()),
            Error(LexicalError::UnclosedStringLiteral)
        ]
    )
}
