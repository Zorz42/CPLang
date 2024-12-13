use crate::compiler::error::{merge_file_positions, CompilerResult, FilePosition};
use crate::compiler::parser::expression::{parse_expression, Expression};
use crate::compiler::parser::function::FunctionSignature;
use crate::compiler::tokenizer::{Symbol, Token, TokenBlock};

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub name: String,
    pub value: Expression,
    pub pos: FilePosition,
}

pub fn parse_variable_declaration(functions: &Vec<FunctionSignature>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<Option<VariableDeclaration>> {
    if *curr_idx + 1 >= block.children.len() {
        return Ok(None);
    }

    let token1 = &block.children[*curr_idx].0;
    let token2 = &block.children[*curr_idx + 1].0;
    let begin_pos = &block.children[*curr_idx].1;

    let name;

    // check if first token is identifier and second token is assignment
    match token1 {
        Token::Identifier(ident) => {
            name = ident.clone();
        },
        _ => return Ok(None),
    }

    match token2 {
        Token::Symbol(symbol) => {
            if symbol == &Symbol::Equal {
                *curr_idx += 2;
            } else {
                return Ok(None);
            }
        },
        _ => return Ok(None),
    }

    let (value, expr_pos) = parse_expression(functions, block, curr_idx)?;

    Ok(Some(VariableDeclaration{
        name,
        value,
        pos: merge_file_positions(begin_pos, &expr_pos),
    }))
}