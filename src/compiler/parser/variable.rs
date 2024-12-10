use crate::compiler::parser::expression::{parse_expression, Expression};
use crate::compiler::tokenizer::{Symbol, Token, TokenBlock};

#[derive(Debug)]
pub struct VariableDeclaration {
    pub name: String,
    pub value: Expression,
}

pub fn parse_variable_declaration(block: &TokenBlock, curr_idx: &mut usize) -> Option<VariableDeclaration> {
    if *curr_idx + 1 >= block.children.len() {
        return None;
    }

    let token1 = &block.children[*curr_idx];
    let token2 = &block.children[*curr_idx + 1];

    let mut name = String::new();

    // check if first token is identifier and second token is assignment
    match token1 {
        Token::Identifier(ident) => {
            name = ident.clone();
        },
        _ => return None,
    }

    match token2 {
        Token::Symbol(symbol) => {
            if symbol == &Symbol::Equal {
                *curr_idx += 2;
            } else {
                return None;
            }
        },
        _ => return None,
    }

    let value = parse_expression(block, curr_idx);

    Some(VariableDeclaration{
        name,
        value,
    })
}