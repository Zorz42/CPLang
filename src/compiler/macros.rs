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

        let (macro_name, name_pos) = match token_block.get() {
            (Token::Identifier(name), pos) => (name, pos),
            (Token::End, _) =>
                return Err(CompilerError {
                    message: "Expected another token after this one".to_string(),
                    position: Some(token_block.get_last_pos()),
                }),
            (_, pos) =>
                return Err(CompilerError {
                    message: "Expected identifier".to_string(),
                    position: Some(pos),
                }),
        };

        let mut macro_arguments = Vec::new();
        let macro_block;

        loop {
            match token_block.get() {
                (Token::Identifier(arg_name), _) => macro_arguments.push(arg_name),
                (Token::BraceBlock(inner_block), _) => {
                    macro_block = insert_macros_block(&macros, inner_block)?;
                    break;
                }
                (_, pos) =>
                    return Err(CompilerError {
                        message: "Expected identifier or brace block".to_string(),
                        position: Some(pos),
                    }),
            }
        }

        if macros.contains_key(&macro_name) {
            return Err(CompilerError {
                message: "Macro redefinition not allowed".to_string(),
                position: Some(name_pos),
            });
        }

        macros.insert(
            macro_name,
            MacroDeclaration {
                arguments: macro_arguments,
                block: macro_block,
            },
        );
    }

    let block = insert_macros_block(&macros, TokenBlock::new(res))?;

    Ok(block.into_iter())
}

fn gen_macro(macro_declaration: &MacroDeclaration, arguments: Vec<TokenBlock>) -> Vec<(Token, FilePosition)> {
    assert_eq!(macro_declaration.arguments.len(), arguments.len());

    gen_block(&macro_declaration.arguments, macro_declaration.block.clone(), &arguments)
}

fn gen_block(arguments: &Vec<String>, block: TokenBlock, argument_values: &Vec<TokenBlock>) -> Vec<(Token, FilePosition)> {
    let mut res = Vec::new();
    for (token, pos) in block.into_iter() {
        match token {
            Token::Identifier(ident) if let Some(pos) = arguments.iter().position(|x| **x == ident) => {
                for i in argument_values[pos].clone().into_iter() {
                    res.push(i);
                }
            }
            Token::BracketBlock(block) => {
                let block = TokenBlock::new(gen_block(arguments, block, argument_values));
                res.push((Token::BracketBlock(block), pos));
            }
            Token::BraceBlock(block) => {
                let block = TokenBlock::new(gen_block(arguments, block, argument_values));
                res.push((Token::BraceBlock(block), pos));
            }
            Token::ParenthesisBlock(block) => {
                let block = TokenBlock::new(gen_block(arguments, block, argument_values));
                res.push((Token::ParenthesisBlock(block), pos));
            }
            _ => res.push((token, pos)),
        }
    }
    res
}

fn insert_macros_block(macros: &HashMap<String, MacroDeclaration>, mut block: TokenBlock) -> CompilerResult<TokenBlock> {
    let mut res = Vec::new();
    while block.has_tokens() {
        let (token, pos) = block.get();
        match token {
            Token::BraceBlock(block) => {
                let block = insert_macros_block(macros, block)?;
                res.push((Token::BraceBlock(block), pos));
            }
            Token::BracketBlock(block) => {
                let block = insert_macros_block(macros, block)?;
                res.push((Token::BracketBlock(block), pos));
            }
            Token::ParenthesisBlock(block) => {
                let block = insert_macros_block(macros, block)?;
                res.push((Token::ParenthesisBlock(block), pos));
            }
            Token::Identifier(macro_name) if let Some(macro_decl) = macros.get(&macro_name) => {
                let mut arguments = Vec::new();
                let mut curr_arg = Vec::new();
                while arguments.len() < macro_decl.arguments.len() {
                    let (token, pos2) = block.get();
                    match token {
                        Token::Semicolon => {
                            arguments.push(curr_arg);
                            curr_arg = Vec::new();
                        }
                        Token::End => {
                            return Err(CompilerError {
                                message: "Too few arguments for this macro call".to_owned(),
                                position: Some(pos),
                            });
                        }
                        _ => curr_arg.push((token, pos2)),
                    }
                }

                let arguments = arguments.into_iter().map(TokenBlock::new).collect::<Vec<_>>();
                let mut res_block = gen_macro(macro_decl, arguments);
                res.append(&mut res_block);
            }
            _ => res.push((token, pos)),
        }
    }
    Ok(TokenBlock::new(res))
}
