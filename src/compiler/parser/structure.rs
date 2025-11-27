use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::parser::ast::{ASTStructDeclaration, ASTType};
use crate::compiler::parser::block::parse_block;
use crate::compiler::parser::function::parse_function_declaration;
use crate::compiler::parser::typed::parse_type;
use crate::compiler::tokenizer::{Keyword, Symbol, Token, TokenBlock};

pub fn parse_struct_declaration(block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<Option<ASTStructDeclaration>> {
    if Some(Token::Keyword(Keyword::Struct)) != block.children.get(*curr_idx).map(|x| x.0.clone()) {
        return Ok(None);
    }

    *curr_idx += 1;

    let name = match block.children.get(*curr_idx).map(|x| &x.0) {
        Some(Token::Identifier(name)) => name.clone(),
        _ => {
            return Err(CompilerError {
                message: "Expected struct name after struct keyword".to_owned(),
                position: Some(block.children[*curr_idx - 1].1.clone()),
            });
        }
    };

    *curr_idx += 1;

    let block = match block.children.get(*curr_idx).map(|x| &x.0) {
        Some(Token::BraceBlock(block)) => block.clone(),
        _ => {
            return Err(CompilerError {
                message: "Expected block after struct name".to_owned(),
                position: Some(block.children[*curr_idx - 1].1.clone()),
            });
        }
    };

    *curr_idx += 1;
    // parse struct body

    let mut idx = 0;
    let mut fields = Vec::new();
    let mut methods = Vec::new();

    while idx < block.children.len() {
        match block.children.get(idx).map(|x| &x.0) {
            Some(Token::Identifier(name)) => {
                idx += 1;

                let type_hint = if let Some(Token::Symbol(Symbol::Colon)) = block.children.get(idx).map(|x| &x.0) {
                    idx += 1;
                    parse_type(&block, &mut idx)?
                } else {
                    ASTType::Any(FilePosition::unknown())
                };

                fields.push((name.clone(), type_hint));
            }
            Some(Token::Keyword(Keyword::Fn)) => {
                idx += 1;
                let (signature, block) = parse_function_declaration(&block, &mut idx)?;
                let block = parse_block(&Vec::new(), &block)?;
                methods.push((signature, block));
            }
            _ => {
                return Err(CompilerError {
                    message: "Expected field name of fn keyword".to_owned(),
                    position: Some(block.children[idx].1.clone()),
                });
            }
        }
    }

    Ok(Some(ASTStructDeclaration { name, fields, methods }))
}
