use crate::lexer::token::TokenType;
use crate::parser::{Parser, SyntaxError};

#[derive(Debug, PartialEq)]
pub enum Expr {
    Literal(TokenType),
    Identifier(String),
    Binary(Box<Expr>, TokenType, Box<Expr>),
    Unary(TokenType, Box<Expr>),
    Assign(Box<Expr>, TokenType, Box<Expr>),
    Grouping(Box<Expr>),
}

impl<'a> Parser<'a> {
    pub fn parse_expr(&mut self) -> Result<Expr, SyntaxError> {
        self.parse_assign_expr()
    }

    fn parse_assign_expr(&mut self) -> Result<Expr, SyntaxError> {
        use TokenType::*;

        let mut expr = self.parse_logical_expr()?;

        while let Some(op) = self.match_token(&[
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
        ]) {
            let right = self.parse_logical_expr()?;
            expr = Expr::Assign(Box::new(expr), op.value, Box::new(right));
        }

        Ok(expr)
    }

    fn parse_logical_expr(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.parse_bit_or_expr()?;

        while let Some(op) = self.match_token(&[TokenType::OpOr, TokenType::OpAnd]) {
            let right = self.parse_bit_or_expr()?;
            expr = Expr::Binary(Box::new(expr), op.value, Box::new(right));
        }

        Ok(expr)
    }

    fn parse_bit_or_expr(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.parse_bit_xor_expr()?;

        while let Some(op) = self.match_token(&[TokenType::OpBwOr]) {
            let right = self.parse_bit_xor_expr()?;
            expr = Expr::Binary(Box::new(expr), op.value, Box::new(right));
        }

        Ok(expr)
    }

    fn parse_bit_xor_expr(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.parse_bit_and_expr()?;

        while let Some(op) = self.match_token(&[TokenType::OpBwXor]) {
            let right = self.parse_bit_and_expr()?;
            expr = Expr::Binary(Box::new(expr), op.value, Box::new(right));
        }

        Ok(expr)
    }

    fn parse_bit_and_expr(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.parse_equality_expr()?;

        while let Some(op) = self.match_token(&[TokenType::OpBwAnd]) {
            let right = self.parse_equality_expr()?;
            expr = Expr::Binary(Box::new(expr), op.value, Box::new(right));
        }

        Ok(expr)
    }

    fn parse_equality_expr(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.parse_relational_expr()?;

        while let Some(op) = self.match_token(&[TokenType::OpEqualEq, TokenType::OpNotEq]) {
            let right = self.parse_relational_expr()?;
            expr = Expr::Binary(Box::new(expr), op.value, Box::new(right));
        }

        Ok(expr)
    }

    fn parse_relational_expr(&mut self) -> Result<Expr, SyntaxError> {
        use TokenType::*;
        let mut expr = self.parse_shift_expr()?;

        while let Some(op) = self.match_token(&[OpLess, OpGreater, OpLessEq, OpGreaterEq]) {
            let right = self.parse_shift_expr()?;
            expr = Expr::Binary(Box::new(expr), op.value, Box::new(right));
        }

        Ok(expr)
    }

    fn parse_shift_expr(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.parse_add_expr()?;

        while let Some(op) = self.match_token(&[TokenType::OpBwLShift, TokenType::OpBwRShift]) {
            let right = self.parse_add_expr()?;
            expr = Expr::Binary(Box::new(expr), op.value, Box::new(right));
        }

        Ok(expr)
    }

    fn parse_add_expr(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.parse_mul_expr()?;

        while let Some(op) = self.match_token(&[TokenType::OpAdd, TokenType::OpSub]) {
            let right = self.parse_mul_expr()?;
            expr = Expr::Binary(Box::new(expr), op.value, Box::new(right));
        }

        Ok(expr)
    }

    fn parse_mul_expr(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.parse_exp_expr()?;

        while let Some(op) =
            self.match_token(&[TokenType::OpMul, TokenType::OpDiv, TokenType::OpMod])
        {
            let right = self.parse_exp_expr()?;
            expr = Expr::Binary(Box::new(expr), op.value, Box::new(right));
        }

        Ok(expr)
    }

    fn parse_exp_expr(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.parse_unary_expr()?;

        while let Some(op) = self.match_token(&[TokenType::OpExp]) {
            let right = self.parse_unary_expr()?;
            expr = Expr::Binary(Box::new(expr), op.value, Box::new(right));
        }

        Ok(expr)
    }

    fn parse_unary_expr(&mut self) -> Result<Expr, SyntaxError> {
        if let Some(op) = self.match_token(&[
            TokenType::OpNot,
            TokenType::OpBwNot,
            TokenType::OpSub,
            TokenType::OpAdd,
        ]) {
            let right = self.parse_unary_expr()?;
            return Ok(Expr::Unary(op.value, Box::new(right)));
        }

        self.parse_primary_expr()
    }

    fn parse_primary_expr(&mut self) -> Result<Expr, SyntaxError> {
        if let Some(token) = self.peek() {
            match &token.value {
                TokenType::LtInteger(_)
                | TokenType::LtFloat(_)
                | TokenType::LtChar(_)
                | TokenType::LtString(_) => return Ok(Expr::Literal(token.value.clone())),
                TokenType::Identifier(name) => return Ok(Expr::Identifier(name.to_string())),
                _ => {}
            }
        }

        if self.match_token(&[TokenType::LParen]).is_some() {
            let expr = self.parse_expr()?;
            self.consume_token(TokenType::RParen)?;
            return Ok(Expr::Grouping(Box::new(expr)));
        }

        Err(SyntaxError::UnexpectedToken(
            self.peek().unwrap().value.clone(),
        ))
    }
}
