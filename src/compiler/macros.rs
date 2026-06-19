use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::tokenizer::{Token, TokenBlock};
use std::collections::HashMap;

struct MacroDeclaration {
    arguments: Vec<String>,
    block: TokenBlock,
}

pub fn insert_macros(tokens: Vec<(Token, FilePosition)>) -> CompilerResult<Vec<(Token, FilePosition)>> {
    let mut token_block = TokenBlock::new(tokens);
    let mut res = Vec::new();
    let mut macros = HashMap::new();

    while token_block.has_tokens() {
        let (token, pos) = token_block.get();

        if token != Token::Macro {
            res.push((token, pos));
            continue;
        }

        let (token, pos) = token_block.get();
        let Token::Identifier(macro_name) = token else {
            return Err(CompilerError {
                message: "Expected identifier".to_string(),
                position: Some(pos),
            })
        };

        let mut macro_arguments = Vec::new();
        let macro_block;

        loop {
            let (token, pos) = token_block.get();

            if let Token::Identifier(arg_name) = token {
                macro_arguments.push(arg_name);
            } else if let Token::BraceBlock(inner_block) = token {
                macro_block = inner_block;
                break;
            } else {
                return Err(CompilerError {
                    message: "Expected identifier or brace block".to_string(),
                    position: Some(pos),
                })
            }
        }

        if macros.contains_key(&macro_name) {
            return Err(CompilerError {
                message: "Macro redefinition not allowed".to_string(),
                position: Some(pos),
            });
        }

        macros.insert(macro_name, MacroDeclaration {
            arguments: macro_arguments,
            block: macro_block,
        });
    }
    Ok(res)
}