use crate::compiler::tokenizer::{TokenBlock, Keyword, Token};

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: String,
    pub block: TokenBlock,
}

pub fn parse_function_declaration(block: &TokenBlock, curr_idx: &mut usize) -> FunctionDeclaration {
    let mut res = FunctionDeclaration {
        name: String::new(),
        block: TokenBlock { children: Vec::new() },
    };

    assert_eq!(block.children[*curr_idx], Token::Keyword(Keyword::Fn));
    *curr_idx += 1;

    match &block.children[*curr_idx] {
        Token::Identifier(name) => {
            res.name = name.clone();
        },
        _ => panic!("Expected function name identifier"),
    }

    *curr_idx += 1;
    match &block.children[*curr_idx] {
        Token::Block(block) => {
            res.block = block.clone();
        },
        _ => panic!("Expected block after function name"),
    }
    *curr_idx += 1;

    res
}