use crate::compiler::error::{CompilerError, CompilerResult};
use crate::compiler::parser::ast::{ASTStatement, ASTStructDeclaration};
use crate::compiler::parser::block::parse_block;
use crate::compiler::parser::expression::parse_expression;
use crate::compiler::tokenizer::{Token, TokenBlock};

pub fn parse_if_statement(structs: &Vec<ASTStructDeclaration>, block: &mut TokenBlock) -> CompilerResult<Option<ASTStatement>> {
    if block.peek().0 != Token::If {
        return Ok(None);
    }
    block.get();
    let condition = parse_expression(structs, block)?;

    let res_block = match block.get() {
        (Token::BraceBlock(token_block), _) => parse_block(structs, token_block)?,
        (_, pos) => {
            return Err(CompilerError {
                message: "Expected block after if condition".to_string(),
                position: Some(pos),
            });
        }
    };

    let else_block = if Token::Else == block.peek().0 {
        block.get();
        match block.get() {
            (Token::BraceBlock(token_block), _) => Some(parse_block(structs, token_block)?),
            (_, pos) => {
                return Err(CompilerError {
                    message: "Expected block after else keyword".to_string(),
                    position: Some(pos),
                });
            }
        }
    } else {
        None
    };

    Ok(Some(ASTStatement::If {
        condition,
        block: res_block,
        else_block,
    }))
}

pub fn parse_while_statement(structs: &Vec<ASTStructDeclaration>, block: &mut TokenBlock) -> CompilerResult<Option<ASTStatement>> {
    if block.peek().0 != Token::While {
        return Ok(None);
    }

    block.get();
    let condition = parse_expression(structs, block)?;

    let res_block = match block.get() {
        (Token::BraceBlock(token_block), _) => parse_block(structs, token_block)?,
        (_, pos) => {
            return Err(CompilerError {
                message: "Expected block after while condition".to_string(),
                position: Some(pos),
            });
        }
    };

    Ok(Some(ASTStatement::While { condition, block: res_block }))
}
