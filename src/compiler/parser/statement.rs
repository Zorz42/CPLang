use crate::compiler::error::{CompilerError, CompilerResult};
use crate::compiler::parser::ast::{ASTStatement, ASTStructDeclaration};
use crate::compiler::parser::block::parse_block;
use crate::compiler::parser::expression::parse_expression;
use crate::compiler::tokenizer::{Token, TokenBlock};

pub fn parse_if_statement(structs: &Vec<ASTStructDeclaration>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<Option<ASTStatement>> {
    if block.children[*curr_idx].0 != Token::If {
        return Ok(None);
    }
    *curr_idx += 1;
    let condition = parse_expression(structs, block, curr_idx)?;

    let res_block;
    if let Token::BraceBlock(token_block) = &block.children[*curr_idx].0 {
        *curr_idx += 1;
        res_block = parse_block(structs, token_block)?;
    } else {
        return Err(CompilerError {
            message: "Expected block after if condition".to_string(),
            position: Some(block.children[*curr_idx].1.clone()),
        });
    }

    let mut else_block = None;
    if let Some(Token::Else) = block.children.get(*curr_idx).map(|x| &x.0) {
        *curr_idx += 1;
        if let Token::BraceBlock(token_block) = &block.children[*curr_idx].0 {
            *curr_idx += 1;
            else_block = Some(parse_block(structs, token_block)?);
        } else {
            return Err(CompilerError {
                message: "Expected block after else keyword".to_string(),
                position: Some(block.children[*curr_idx].1.clone()),
            });
        }
    }

    Ok(Some(ASTStatement::If {
        condition,
        block: res_block,
        else_block,
    }))
}

pub fn parse_while_statement(structs: &Vec<ASTStructDeclaration>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<Option<ASTStatement>> {
    if block.children[*curr_idx].0 != Token::While {
        return Ok(None);
    }

    *curr_idx += 1;
    let condition = parse_expression(structs, block, curr_idx)?;

    let res_block;
    if let Token::BraceBlock(token_block) = &block.children[*curr_idx].0 {
        *curr_idx += 1;
        res_block = parse_block(structs, token_block)?;
    } else {
        return Err(CompilerError {
            message: "Expected block after while condition".to_string(),
            position: Some(block.children[*curr_idx].1.clone()),
        });
    }

    Ok(Some(ASTStatement::While { condition, block: res_block }))
}
