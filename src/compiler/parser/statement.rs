use crate::compiler::error::{CompilerError, CompilerResult};
use crate::compiler::parser::block::{parse_block, Block};
use crate::compiler::parser::expression::{parse_expression, Expression};
use crate::compiler::parser::structure::StructDeclaration;
use crate::compiler::tokenizer::{Keyword, Token, TokenBlock};

#[derive(Clone, Debug)]
pub struct IfStatement {
    pub condition: Expression,
    pub block: Block,
    pub else_block: Option<Block>,
}

pub fn parse_if_statement(structs: &Vec<StructDeclaration>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<Option<IfStatement>> {
    if block.children[*curr_idx].0 != Token::Keyword(Keyword::If) {
        return Ok(None);
    }
    *curr_idx += 1;
    let (condition, _) = parse_expression(structs, block, curr_idx)?;

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
    if let Some(Token::Keyword(Keyword::Else)) = block.children.get(*curr_idx).map(|x| &x.0) {
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

    Ok(Some(IfStatement {
        condition,
        block: res_block,
        else_block,
    }))
}

#[derive(Clone, Debug)]
pub struct WhileStatement {
    pub condition: Expression,
    pub block: Block,
}

pub fn parse_while_statement(structs: &Vec<StructDeclaration>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<Option<WhileStatement>> {
    if block.children[*curr_idx].0 != Token::Keyword(Keyword::While) {
        return Ok(None);
    }

    *curr_idx += 1;
    let (condition, _) = parse_expression(structs, block, curr_idx)?;

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

    Ok(Some(WhileStatement {
        condition,
        block: res_block,
    }))
}