use crate::compiler::error::{CompilerError, CompilerResult};
use crate::compiler::tokenizer::{TokenBlock, Token};

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub args: Vec<String>,
}

pub fn parse_function_declaration(block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<(FunctionSignature, TokenBlock)> {
    let mut res_signature = FunctionSignature {
        name: String::new(),
        args: Vec::new(),
    };
    let res_block;

    match &block.children.get(*curr_idx).map(|x| x.0.clone()) {
        Some(Token::Identifier(name)) => {
            res_signature.name = name.clone();
        },
        _ => return Err(CompilerError {
            message: "Expected function name after".to_string(),
            position: Some(
                block.children[*curr_idx - 1].1.clone()
            ),
        }),
    }
    *curr_idx += 1;

    loop {
        match &block.children.get(*curr_idx).map(|x| x.0.clone()) {
            Some(Token::Block(block)) => {
                res_block = block.clone();
                *curr_idx += 1;
                break;
            },
            Some(Token::Identifier(arg)) => {
                res_signature.args.push(arg.clone());
            },
            _ => return Err(CompilerError {
                message: "Expected block or argument identifier after function name".to_string(),
                position: Some(
                    block.children[*curr_idx - 1].1.clone()
                ),
            }),
        }
        *curr_idx += 1;
    }

    Ok((res_signature, res_block))
}