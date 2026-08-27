use crate::compiler::error::{CompilerError, CompilerResult};
use crate::compiler::parser::ast::{ASTBlock, ASTStatement, ASTStructDeclaration};
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
        (Token::End, _) => {
            return Err(CompilerError {
                message: "Expected another token after this one".to_string(),
                position: Some(block.get_last_pos()),
            });
        }
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
            (Token::BraceBlock(token_block), _) => parse_block(structs, token_block)?,
            (Token::End, _) => {
                return Err(CompilerError {
                    message: "Expected another token after this one".to_string(),
                    position: Some(block.get_last_pos()),
                });
            }
            (_, pos) => {
                return Err(CompilerError {
                    message: "Expected block after else keyword".to_string(),
                    position: Some(pos),
                });
            }
        }
    } else {
        ASTBlock {
            children: Vec::new(),
        }
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
        (Token::End, _) => {
            return Err(CompilerError {
                message: "Expected another token after this one".to_string(),
                position: Some(block.get_last_pos()),
            });
        }
        (_, pos) => {
            return Err(CompilerError {
                message: "Expected block after while condition".to_string(),
                position: Some(pos),
            });
        }
    };

    Ok(Some(ASTStatement::While { condition, block: res_block }))
}

pub fn parse_for_statement(structs: &Vec<ASTStructDeclaration>, block: &mut TokenBlock) -> CompilerResult<Option<ASTStatement>> {
    if block.peek().0 != Token::For {
        return Ok(None);
    }

    let mut pos = block.get().1;
    let iterator = match block.get() {
        (Token::Identifier(ident), pos2) => {
            pos += pos2;
            ident
        }
        (Token::End, _) => {
            return Err(CompilerError {
                message: "Expected another token after this one".to_string(),
                position: Some(block.get_last_pos()),
            });
        }
        (_, pos) => {
            return Err(CompilerError {
                message: "Expected identifier after for keyword".to_string(),
                position: Some(pos),
            });
        }
    };
    let element = parse_expression(structs, block)?;
    pos += element.pos;

    let res_block = match block.get() {
        (Token::BraceBlock(token_block), pos2) => {
            pos += pos2;
            parse_block(structs, token_block)?
        }
        (Token::End, _) => {
            return Err(CompilerError {
                message: "Expected another token after this one".to_string(),
                position: Some(block.get_last_pos()),
            });
        }
        (_, pos) => {
            return Err(CompilerError {
                message: "Expected block after for statement".to_string(),
                position: Some(pos),
            });
        }
    };

    Ok(Some(ASTStatement::For {
        iterator,
        element,
        block: res_block,
        pos,
    }))
}

pub fn parse_break_statement(_: &Vec<ASTStructDeclaration>, block: &mut TokenBlock) -> CompilerResult<Option<ASTStatement>> {
    if block.peek().0 != Token::Break {
        return Ok(None);
    }

    let (_, mut pos) = block.get();

    let depth = if let (Token::ConstInteger32(depth), pos2) = block.peek().clone() {
        block.get();
        pos += pos2;
        if depth <= 0 {
            return Err(CompilerError {
                message: "Break depth has to be positive".to_owned(),
                position: Some(pos2),
            });
        }
        depth
    } else {
        1
    };

    Ok(Some(ASTStatement::Break { depth, pos }))
}

pub fn parse_continue_statement(_: &Vec<ASTStructDeclaration>, block: &mut TokenBlock) -> CompilerResult<Option<ASTStatement>> {
    if block.peek().0 != Token::Continue {
        return Ok(None);
    }

    let (_, mut pos) = block.get();

    let depth = if let (Token::ConstInteger32(depth), pos2) = block.peek().clone() {
        block.get();
        pos += pos2;
        if depth <= 0 {
            return Err(CompilerError {
                message: "Continue depth has to be positive".to_owned(),
                position: Some(pos2),
            });
        }
        depth
    } else {
        1
    };

    Ok(Some(ASTStatement::Continue { depth, pos }))
}
