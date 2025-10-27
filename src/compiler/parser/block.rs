use crate::compiler::error::CompilerResult;
use crate::compiler::parser::expression::parse_expression;
use crate::compiler::parser::function::{parse_return_statement};
use crate::compiler::parser::inline_c::parse_inline_c;
use crate::compiler::parser::out::parse_out_statement;
use crate::compiler::parser::Statement;
use crate::compiler::parser::statement::{parse_if_statement, parse_while_statement};
use crate::compiler::parser::structure::StructDeclaration;
use crate::compiler::parser::variable::parse_variable_declaration;
use crate::compiler::tokenizer::{Token, TokenBlock};

#[derive(Debug, Clone)]
pub struct Block {
    pub children: Vec<Statement>,
}

pub fn parse_block(structs: &Vec<StructDeclaration>, block: &TokenBlock) -> CompilerResult<Block> {
    let mut curr_idx = 0;

    let mut res = Block {
        children: Vec::new(),
    };

    while curr_idx < block.children.len() {
        match &block.children[curr_idx].0 {
            Token::BraceBlock(sub_block) => {
                res.children.push(Statement::Block(parse_block(structs, sub_block)?));
                curr_idx += 1;
            },
            _ => {
                if let Some(statement) = parse_return_statement(structs, block, &mut curr_idx)? {
                    res.children.push(statement);
                } else if let Some(statement) = parse_variable_declaration(structs, block, &mut curr_idx)? {
                    res.children.push(Statement::VariableDeclaration(statement));
                } else if let Some(statement) = parse_out_statement(structs, block, &mut curr_idx)? {
                    res.children.push(Statement::Print(statement));
                } else if let Some(statement) = parse_if_statement(structs, block, &mut curr_idx)? {
                    res.children.push(Statement::If(statement));
                } else if let Some(statement) = parse_while_statement(structs, block, &mut curr_idx)? {
                    res.children.push(Statement::While(statement));
                } else if let Some(statement) = parse_inline_c(block, &mut curr_idx)? {
                    res.children.push(Statement::InlineC(statement));
                } else {
                    let (expression, _) = parse_expression(structs, block, &mut curr_idx)?;
                    res.children.push(Statement::Expression(expression));
                }
            },
        }
    }

    Ok(res)
}