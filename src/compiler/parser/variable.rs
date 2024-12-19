use crate::compiler::error::{merge_file_positions, CompilerResult, FilePosition};
use crate::compiler::parser::expression::{parse_expression, Expression};
use crate::compiler::parser::function::FunctionSignature;
use crate::compiler::parser::structure::StructDeclaration;
use crate::compiler::tokenizer::{Symbol, Token, TokenBlock};

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub assign_to: Expression,
    pub value: Expression,
    pub pos: FilePosition,
}

pub fn parse_variable_declaration(functions: &Vec<FunctionSignature>, structs: &Vec<StructDeclaration>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<Option<VariableDeclaration>> {
    let old_idx = *curr_idx;
    if *curr_idx + 1 >= block.children.len() {
        return Ok(None);
    }

    let (expr1, expr1_pos) = match parse_expression(functions, structs, block, curr_idx) {
        Ok(x) => x,
        Err(_) => {
            *curr_idx = old_idx;
            return Ok(None);
        }
    };

    if Some(Token::Symbol(Symbol::Assign)) != block.children.get(*curr_idx).map(|x| x.0.clone()) {
        *curr_idx = old_idx;
        return Ok(None);
    }

    *curr_idx += 1;

    let (expr2, expr2_pos) = parse_expression(functions, structs, block, curr_idx)?;

    Ok(Some(VariableDeclaration{
        assign_to: expr1,
        value: expr2,
        pos: merge_file_positions(&expr1_pos, &expr2_pos),
    }))
}