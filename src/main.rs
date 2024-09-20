use crate::{lexer::Lexer, parser::Parser};

mod lexer;
mod parser;

fn main() {
    use std::io;

    loop {
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        let lexer = Lexer::from(input.as_str());
        let mut parser = Parser::new(lexer);

        // FIX: shit ain't working
        dbg!(parser.parse_expr());
    }
}
