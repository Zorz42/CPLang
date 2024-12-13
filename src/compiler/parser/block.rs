use crate::compiler::error::CompilerResult;
use crate::compiler::parser::expression::parse_expression;
use crate::compiler::parser::function::FunctionSignature;
use crate::compiler::parser::print::parse_print_statement;
use crate::compiler::parser::return_statement::parse_return_statement;
use crate::compiler::parser::Statement;
use crate::compiler::parser::variable::parse_variable_declaration;
use crate::compiler::tokenizer::{Token, TokenBlock};

#[derive(Debug, Clone)]
pub struct Block {
    pub children: Vec<Statement>,
}

pub fn parse_block(functions: &Vec<FunctionSignature>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<Block> {
    let mut res = Block {
        children: Vec::new(),
    };

    while *curr_idx < block.children.len() {
        match &block.children[*curr_idx].0 {
            Token::Block(sub_block) => {
                let mut idx = 0;
                res.children.push(Statement::Block(parse_block(functions, sub_block, &mut idx)?));
                *curr_idx += 1;
            },
            _ => {
                if let Some(statement) = parse_return_statement(functions, block, curr_idx)? {
                    res.children.push(statement);
                } else if let Some(statement) = parse_variable_declaration(functions, block, curr_idx)? {
                    res.children.push(Statement::VariableDeclaration(statement));
                } else if let Some(statement) = parse_print_statement(functions, block, curr_idx)? {
                    res.children.push(Statement::Print(statement));
                } else {
                    let expression = parse_expression(functions, block, curr_idx)?;
                    res.children.push(Statement::Expression(expression));
                }
            },
        }
    }

    Ok(res)
}