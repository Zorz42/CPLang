use crate::compiler::error::{merge_file_positions, CompilerError, CompilerResult, FilePosition};
use crate::compiler::parser::expression::{parse_expression, Expression};
use crate::compiler::parser::function::FunctionSignature;
use crate::compiler::parser::structure::StructDeclaration;
use crate::compiler::tokenizer::{Symbol, Token, TokenBlock};

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub name: Vec<String>,
    pub value: Expression,
    pub pos: FilePosition,
}

pub fn parse_variable_declaration(functions: &Vec<FunctionSignature>, structs: &Vec<StructDeclaration>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<Option<VariableDeclaration>> {
    let old_idx = *curr_idx;
    if *curr_idx + 1 >= block.children.len() {
        return Ok(None);
    }

    let mut name = Vec::new();

    // check if first token is identifier and second token is assignment
    match block.children.get(*curr_idx).map(|x| x.0.clone()) {
        Some(Token::Identifier(ident)) => {
            name.push(ident.clone());
        },
        _ => return Ok(None),
    }

    let begin_pos = &block.children[*curr_idx].1;
    *curr_idx += 1;

    while let Some(Token::Symbol(Symbol::Dot)) = block.children.get(*curr_idx).map(|x| x.0.clone()) {
        *curr_idx += 1;
        match block.children.get(*curr_idx).map(|x| x.0.clone()) {
            Some(Token::Identifier(ident)) => {
                name.push(ident.clone());
                *curr_idx += 1;
            },
            _ => return Err(CompilerError {
                message: "Expected identifier after dot".to_string(),
                position: Some(block.children[*curr_idx - 1].1.clone()),
            })
        }
    }

    if Some(Token::Symbol(Symbol::Assign)) != block.children.get(*curr_idx).map(|x| x.0.clone()) {
        *curr_idx = old_idx;
        return Ok(None);
    }

    *curr_idx += 1;

    let (value, expr_pos) = parse_expression(functions, structs, block, curr_idx)?;

    Ok(Some(VariableDeclaration{
        name,
        value,
        pos: merge_file_positions(begin_pos, &expr_pos),
    }))
}