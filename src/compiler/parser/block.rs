use crate::compiler::error::CompilerResult;
use crate::compiler::parser::assignment::parse_assignment;
use crate::compiler::parser::ast::{ASTBlock, ASTStatement, ASTStructDeclaration};
use crate::compiler::parser::expression::parse_expression;
use crate::compiler::parser::function::parse_return_statement;
use crate::compiler::parser::out::parse_out_statement;
use crate::compiler::parser::statement::{parse_if_statement, parse_while_statement};
use crate::compiler::tokenizer::{Token, TokenBlock};

pub fn parse_block(structs: &Vec<ASTStructDeclaration>, block: &TokenBlock) -> CompilerResult<ASTBlock> {
    let mut curr_idx = 0;

    let mut res = ASTBlock { children: Vec::new() };

    while curr_idx < block.children.len() {
        let statement = match &block.children[curr_idx].0 {
            Token::BraceBlock(sub_block) => {
                let statement = ASTStatement::Block {
                    block: parse_block(structs, sub_block)?,
                };
                curr_idx += 1;
                statement
            }
            _ => {
                if let Some(statement) = parse_return_statement(structs, block, &mut curr_idx)? {
                    statement
                } else if let Some(statement) = parse_assignment(structs, block, &mut curr_idx)? {
                    statement
                } else if let Some(statement) = parse_out_statement(structs, block, &mut curr_idx)? {
                    statement
                } else if let Some(statement) = parse_if_statement(structs, block, &mut curr_idx)? {
                    statement
                } else if let Some(statement) = parse_while_statement(structs, block, &mut curr_idx)? {
                    statement
                } else {
                    let expression = parse_expression(structs, block, &mut curr_idx)?;
                    ASTStatement::Expression { expression }
                }
            }
        };
        res.children.push(statement);
    }

    Ok(res)
}
