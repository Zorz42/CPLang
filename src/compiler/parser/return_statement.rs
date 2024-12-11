use crate::compiler::parser::expression::parse_expression;
use crate::compiler::parser::function::FunctionSignature;
use crate::compiler::parser::Statement;
use crate::compiler::tokenizer::{Keyword, Token, TokenBlock};

pub fn parse_return_statement(functions: &Vec<FunctionSignature>, block: &TokenBlock, curr_idx: &mut usize) -> Option<Statement> {
    if block.children[*curr_idx] != Token::Keyword(Keyword::Return) {
        return None;
    }
    *curr_idx += 1;
    let expression = parse_expression(functions, block, curr_idx);
    Some(Statement::Return(expression))
}