use crate::compiler::error::{merge_file_positions, CompilerError, CompilerResult};
use crate::compiler::parser::ast::{ASTFunctionSignature, ASTStatement, ASTStructDeclaration, ASTType};
use crate::compiler::parser::expression::parse_expression;
use crate::compiler::parser::typed::parse_type;
use crate::compiler::tokenizer::{Keyword, Symbol, Token, TokenBlock};

pub fn parse_function_declaration(block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<(ASTFunctionSignature, TokenBlock)> {
    let mut res_signature = ASTFunctionSignature {
        name: String::new(),
        args: Vec::new(),
    };
    let res_block;

    match &block.children.get(*curr_idx).map(|x| x.0.clone()) {
        Some(Token::Identifier(name)) => {
            res_signature.name = name.clone();
        }
        _ => {
            return Err(CompilerError {
                message: "Unexpected token".to_string(),
                position: Some(block.children[*curr_idx].1.clone()),
            });
        }
    }
    *curr_idx += 1;

    loop {
        let (arg, arg_pos) = match &block.children.get(*curr_idx).map(|x| x.0.clone()) {
            Some(Token::BraceBlock(block)) => {
                res_block = block.clone();
                *curr_idx += 1;
                break;
            }
            Some(Token::Identifier(arg)) => (arg.clone(), block.children[*curr_idx].1.clone()),
            _ => {
                return Err(CompilerError {
                    message: "Expected block or argument identifier after function signature".to_string(),
                    position: Some(block.children[*curr_idx - 1].1.clone()),
                });
            }
        };
        *curr_idx += 1;

        let type_hint = match &block.children.get(*curr_idx).map(|x| x.0.clone()) {
            Some(Token::Symbol(Symbol::Colon)) => {
                // type hint
                *curr_idx += 1;
                parse_type(block, curr_idx)?
            }
            _ => ASTType::Any,
        };

        res_signature.args.push((arg, type_hint, arg_pos));
    }

    Ok((res_signature, res_block))
}

pub fn parse_return_statement(structs: &Vec<ASTStructDeclaration>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<Option<ASTStatement>> {
    if block.children[*curr_idx].0 != Token::Keyword(Keyword::Return) {
        return Ok(None);
    }
    let pos1 = block.children[*curr_idx].1.clone();
    *curr_idx += 1;
    if block.children.len() == *curr_idx {
        return Ok(Some(ASTStatement::Return { return_value: None, pos: pos1 }));
    }

    let (expression, pos2) = parse_expression(structs, block, curr_idx)?;
    Ok(Some(ASTStatement::Return {
        return_value: Some(expression),
        pos: merge_file_positions(&pos1, &pos2),
    }))
}
