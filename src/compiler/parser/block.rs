use crate::compiler::error::CompilerResult;
use crate::compiler::parser::expression::parse_expression;
use crate::compiler::parser::function::{parse_return_statement};
use crate::compiler::parser::out::parse_out_statement;
use crate::compiler::parser::statement::{parse_if_statement, parse_while_statement};
use crate::compiler::parser::assignment::parse_assignment;
use crate::compiler::parser::ast::{ASTBlock, ASTStatement, StructDeclaration};
use crate::compiler::tokenizer::{Token, TokenBlock};

pub fn parse_block(structs: &Vec<StructDeclaration>, block: &TokenBlock) -> CompilerResult<ASTBlock> {
    let mut curr_idx = 0;

    let mut res = ASTBlock {
        children: Vec::new(),
    };

    while curr_idx < block.children.len() {
        match &block.children[curr_idx].0 {
            Token::BraceBlock(sub_block) => {
                res.children.push(ASTStatement::Block(parse_block(structs, sub_block)?));
                curr_idx += 1;
            },
            _ => {
                if let Some(statement) = parse_return_statement(structs, block, &mut curr_idx)? {
                    res.children.push(statement);
                } else if let Some(statement) = parse_assignment(structs, block, &mut curr_idx)? {
                    res.children.push(ASTStatement::Assignment(statement));
                } else if let Some(statement) = parse_out_statement(structs, block, &mut curr_idx)? {
                    res.children.push(ASTStatement::Print(statement));
                } else if let Some(statement) = parse_if_statement(structs, block, &mut curr_idx)? {
                    res.children.push(ASTStatement::If(statement));
                } else if let Some(statement) = parse_while_statement(structs, block, &mut curr_idx)? {
                    res.children.push(ASTStatement::While(statement));
                } else {
                    let (expression, _) = parse_expression(structs, block, &mut curr_idx)?;
                    res.children.push(ASTStatement::Expression(expression));
                }
            },
        }
    }

    Ok(res)
}