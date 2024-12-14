use crate::compiler::error::{merge_file_positions, CompilerError, CompilerResult, FilePosition};
use crate::compiler::parser::block::{parse_block, Block};
use crate::compiler::parser::expression::{parse_expression, Expression};
use crate::compiler::parser::function::FunctionSignature;
use crate::compiler::tokenizer::{Keyword, Token, TokenBlock};

#[derive(Clone, Debug)]
pub struct IfStatement {
    pub condition: Expression,
    pub block: Block,
    pub pos: FilePosition,
    pub else_block: Option<Block>,
}

pub fn parse_if_statement(functions: &Vec<FunctionSignature>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<Option<IfStatement>> {
    if block.children[*curr_idx].0 != Token::Keyword(Keyword::If) {
        return Ok(None);
    }
    let pos1 = block.children[*curr_idx].1.clone();
    *curr_idx += 1;
    let (condition, pos2) = parse_expression(functions, block, curr_idx)?;

    let res_block;
    if let Token::Block(token_block) = &block.children[*curr_idx].0 {
        *curr_idx += 1;
        res_block = parse_block(functions, token_block)?;
    } else {
        return Err(CompilerError {
            message: "Expected block after if condition".to_string(),
            position: Some(block.children[*curr_idx].1.clone()),
        });
    }

    let mut else_block = None;
    if let Some(Token::Keyword(Keyword::Else)) = block.children.get(*curr_idx).map(|x| &x.0) {
        *curr_idx += 1;
        if let Token::Block(token_block) = &block.children[*curr_idx].0 {
            *curr_idx += 1;
            else_block = Some(parse_block(functions, token_block)?);
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
        pos: merge_file_positions(&pos1, &pos2),
        else_block,
    }))
}

#[derive(Clone, Debug)]
pub struct WhileStatement {
    pub condition: Expression,
    pub block: Block,
    pub pos: FilePosition,
}

pub fn parse_while_statement(functions: &Vec<FunctionSignature>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<Option<WhileStatement>> {
    if block.children[*curr_idx].0 != Token::Keyword(Keyword::While) {
        return Ok(None);
    }
    let pos1 = block.children[*curr_idx].1.clone();
    *curr_idx += 1;
    let (condition, pos2) = parse_expression(functions, block, curr_idx)?;

    let res_block;
    if let Token::Block(token_block) = &block.children[*curr_idx].0 {
        *curr_idx += 1;
        res_block = parse_block(functions, token_block)?;
    } else {
        return Err(CompilerError {
            message: "Expected block after while condition".to_string(),
            position: Some(block.children[*curr_idx].1.clone()),
        });
    }

    Ok(Some(WhileStatement {
        condition,
        block: res_block,
        pos: merge_file_positions(&pos1, &pos2),
    }))
}