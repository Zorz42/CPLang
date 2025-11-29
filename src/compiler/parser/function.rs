use crate::compiler::error::{merge_file_positions, CompilerError, CompilerResult, FilePosition};
use crate::compiler::parser::ast::{ASTFunctionSignature, ASTStatement, ASTStructDeclaration, ASTType};
use crate::compiler::parser::expression::parse_expression;
use crate::compiler::parser::typed::parse_type;
use crate::compiler::tokenizer::{Token, TokenBlock};

pub fn parse_function_declaration(block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<(ASTFunctionSignature, TokenBlock)> {
    let mut res_signature = ASTFunctionSignature {
        name: String::new(),
        args: Vec::new(),
        template: Vec::new(),
        pos: FilePosition::unknown(),
    };
    let res_block;

    match &block.children.get(*curr_idx).clone() {
        Some((Token::Identifier(name), pos)) => {
            res_signature.name = name.clone();
            res_signature.pos = merge_file_positions(&res_signature.pos, pos);
        }
        _ => {
            return Err(CompilerError {
                message: "Unexpected token".to_string(),
                position: Some(block.children[*curr_idx].1.clone()),
            });
        }
    }
    *curr_idx += 1;

    // check for template declaration
    if let Some((Token::BracketBlock(block), _pos)) = block.children.get(*curr_idx) {
        *curr_idx += 1;
        for (token, token_pos) in block.children.clone() {
            if let Token::Identifier(name) = token {
                res_signature.template.push((name, token_pos));
            } else {
                return Err(CompilerError {
                    message: "Unexpected token, expected identifier".to_string(),
                    position: Some(token_pos),
                });
            }
        }
    }

    loop {
        let (arg, arg_pos) = match block.children.get(*curr_idx).cloned() {
            Some((Token::BraceBlock(block), _pos)) => {
                res_block = block;
                *curr_idx += 1;
                break;
            }
            Some((Token::Identifier(arg), pos)) => {
                res_signature.pos = merge_file_positions(&res_signature.pos, &pos);
                (arg, block.children[*curr_idx].1.clone())
            }
            _ => {
                let pos = if let Some((_, pos)) = block.children.get(*curr_idx) {
                    pos.clone()
                } else {
                    block.children[*curr_idx - 1].1.clone()
                };
                return Err(CompilerError {
                    message: "Expected block or argument identifier after function signature".to_string(),
                    position: Some(pos),
                });
            }
        };
        *curr_idx += 1;

        let type_hint = match &block.children.get(*curr_idx).clone() {
            Some((Token::Colon, pos)) => {
                // type hint
                *curr_idx += 1;
                res_signature.pos = merge_file_positions(&res_signature.pos, pos);
                parse_type(block, curr_idx)?
            }
            _ => ASTType::Any(block.children[*curr_idx].1.clone()),
        };

        res_signature.args.push((arg, type_hint, arg_pos));
    }

    Ok((res_signature, res_block))
}

pub fn parse_return_statement(structs: &Vec<ASTStructDeclaration>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<Option<ASTStatement>> {
    if block.children[*curr_idx].0 != Token::Return {
        return Ok(None);
    }
    let pos1 = block.children[*curr_idx].1.clone();
    *curr_idx += 1;
    if block.children.len() == *curr_idx {
        return Ok(Some(ASTStatement::Return { return_value: None, pos: pos1 }));
    }

    let expression = parse_expression(structs, block, curr_idx)?;
    let pos2 = expression.get_pos();
    Ok(Some(ASTStatement::Return {
        return_value: Some(expression),
        pos: merge_file_positions(&pos1, &pos2),
    }))
}
