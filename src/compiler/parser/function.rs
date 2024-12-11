use crate::compiler::tokenizer::{TokenBlock, Keyword, Token};

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub args: Vec<String>,
}

pub fn parse_function_declaration(block: &TokenBlock, curr_idx: &mut usize) -> (FunctionSignature, TokenBlock) {
    let mut res_signature = FunctionSignature {
        name: String::new(),
        args: Vec::new(),
    };
    let res_block;

    assert_eq!(block.children[*curr_idx], Token::Keyword(Keyword::Fn));
    *curr_idx += 1;

    match &block.children[*curr_idx] {
        Token::Identifier(name) => {
            res_signature.name = name.clone();
        },
        _ => panic!("Expected function name identifier"),
    }
    *curr_idx += 1;

    loop {
        match &block.children[*curr_idx] {
            Token::Block(block) => {
                res_block = block.clone();
                *curr_idx += 1;
                break;
            },
            Token::Identifier(arg) => {
                res_signature.args.push(arg.clone());
            },
            _ => panic!("Expected block after function name"),
        }
        *curr_idx += 1;
    }

    (res_signature, res_block)
}