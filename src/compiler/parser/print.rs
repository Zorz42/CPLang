use crate::compiler::parser::expression::Expression;
use crate::compiler::tokenizer::{Constant, Keyword, Token, TokenBlock};

#[derive(Debug)]
pub struct PrintStatement {
    pub values: Vec<Expression>,
}

pub fn parse_print_statement(block: &TokenBlock, curr_idx: &mut usize) -> Option<PrintStatement> {
    if block.children[*curr_idx] == Token::Keyword(Keyword::Print) {
        *curr_idx += 1;
        match &block.children[*curr_idx] {
            Token::Constant(Constant::String(string)) => {
                *curr_idx += 1;
                Some(PrintStatement {
                    values: vec![Expression::String(string.clone())],
                })
            },
            _ => panic!("Expected string after print keyword"),
        }
    } else {
        None
    }
}