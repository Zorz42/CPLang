use crate::compiler::error::{merge_file_positions, CompilerResult, FilePosition};
use crate::compiler::parser::expression::{parse_expression, Expression, Operator};
use crate::compiler::parser::structure::StructDeclaration;
use crate::compiler::tokenizer::{Symbol, Token, TokenBlock};

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub assign_to: Expression,
    pub value: Expression,
    pub pos: FilePosition,
}

pub fn parse_variable_declaration(structs: &Vec<StructDeclaration>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<Option<VariableDeclaration>> {
    let old_idx = *curr_idx;
    if *curr_idx + 1 >= block.children.len() {
        return Ok(None);
    }

    let (expr1, expr1_pos) = match parse_expression(structs, block, curr_idx) {
        Ok(x) => x,
        Err(_) => {
            *curr_idx = old_idx;
            return Ok(None);
        }
    };

    let symbol = if let Some(Token::Symbol(symbol)) = block.children.get(*curr_idx).map(|x| x.0.clone()) {
        if symbol == Symbol::Assign || symbol == Symbol::Increase || symbol == Symbol::Decrease || symbol == Symbol::Increment || symbol == Symbol::Decrement {
            symbol
        } else {
            *curr_idx = old_idx;
            return Ok(None);
        }
    } else {
        *curr_idx = old_idx;
        return Ok(None);
    };

    *curr_idx += 1;

    let (expr2, expr2_pos) = match symbol {
        Symbol::Assign => {
            parse_expression(structs, block, curr_idx)?
        },
        Symbol::Increase => {
            todo!()
        },
        Symbol::Decrease => {
            todo!()
        },
        Symbol::Increment => {
            todo!()
        },
        Symbol::Decrement => {
            todo!()
        },
        _ => unreachable!()
    };

    Ok(Some(VariableDeclaration{
        assign_to: expr1,
        value: expr2,
        pos: merge_file_positions(&expr1_pos, &expr2_pos),
    }))
}