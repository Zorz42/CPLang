use crate::compiler::error::{CompilerError, CompilerResult};
use crate::compiler::tokenizer::{Keyword, Token, TokenBlock};

#[derive(Debug, Clone)]
pub struct StructDeclaration {
    pub name: String,
    pub fields: Vec<String>,
}

pub fn parse_struct_declaration(block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<Option<StructDeclaration>> {
    if Some(Token::Keyword(Keyword::Struct)) != block.children.get(*curr_idx).map(|x| x.0.clone()) {
        return Ok(None);
    }

    *curr_idx += 1;

    let name = match block.children.get(*curr_idx).map(|x| &x.0) {
        Some(Token::Identifier(name)) => name.clone(),
        _ => return Err(CompilerError {
            message: "Expected struct name after struct keyword".to_owned(),
            position: Some(block.children[*curr_idx - 1].1.clone()),
        }),
    };

    *curr_idx += 1;

    let block = match block.children.get(*curr_idx).map(|x| &x.0) {
        Some(Token::Block(block)) => block.clone(),
        _ => return Err(CompilerError {
            message: "Expected block after struct name".to_owned(),
            position: Some(block.children[*curr_idx - 1].1.clone()),
        }),
    };

    *curr_idx += 1;
    // parse struct body

    let mut idx = 0;
    let mut fields = Vec::new();

    while idx < block.children.len() {
        match block.children.get(idx).map(|x| &x.0) {
            Some(Token::Identifier(name)) => {
                fields.push(name.clone());
            },
            _ => return Err(CompilerError {
                message: "Expected field name".to_owned(),
                position: Some(block.children[idx].1.clone()),
            }),
        }
        idx += 1;
    }

    Ok(Some(
        StructDeclaration {
            name,
            fields,
        }
    ))
}