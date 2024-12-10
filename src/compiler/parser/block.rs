use crate::compiler::parser::expression::parse_expression;
use crate::compiler::parser::Statement;
use crate::compiler::parser::variable::parse_variable_declaration;
use crate::compiler::tokenizer::{Token, TokenBlock};

#[derive(Debug)]
pub struct Block {
    pub children: Vec<Statement>,
}

pub fn parse_block(block: &TokenBlock, curr_idx: &mut usize) -> Block {
    let mut res = Block {
        children: Vec::new(),
    };

    while *curr_idx < block.children.len() {
        match &block.children[*curr_idx] {
            Token::Block(sub_block) => {
                let mut idx = 0;
                res.children.push(Statement::Block(parse_block(sub_block, &mut idx)));
                *curr_idx += 1;
            },
            _ => {
                if let Some(statement) = parse_variable_declaration(block, curr_idx) {
                    res.children.push(Statement::VariableDeclaration(statement));
                } else {
                    let expression = parse_expression(block, curr_idx);
                    res.children.push(Statement::Expression(expression));
                }
            },
        }
    }

    res
}