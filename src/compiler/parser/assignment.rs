use crate::compiler::error::{merge_file_positions, CompilerResult, FilePosition};
use crate::compiler::parser::expression::{parse_expression, Expression};
use crate::compiler::parser::structure::StructDeclaration;
use crate::compiler::tokenizer::{Symbol, Token, TokenBlock};

#[derive(Debug, Clone)]
pub enum AssignmentType {
    Assign,
    Increase,
    Decrease,
    Increment,
    Decrement,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub assign_to: Expression,
    pub value: Expression,
    pub typ: AssignmentType,
    pub pos: FilePosition,
}

pub fn parse_assignment(structs: &Vec<StructDeclaration>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<Option<Assignment>> {
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
        match symbol {
            Symbol::Assign |Symbol::Increase | Symbol::Decrease | Symbol::Increment | Symbol::Decrement => symbol,
            _ => {
                *curr_idx = old_idx;
                return Ok(None);
            }
        }
    } else {
        *curr_idx = old_idx;
        return Ok(None);
    };

    *curr_idx += 1;

    let (expr2, expr2_pos) = parse_expression(structs, block, curr_idx)?;
    let typ = match symbol {
        Symbol::Assign => AssignmentType::Assign,
        Symbol::Increase => AssignmentType::Increase,
        Symbol::Decrease => AssignmentType::Decrease,
        Symbol::Increment => AssignmentType::Increment,
        Symbol::Decrement => AssignmentType::Decrement,
        _ => unreachable!(),
    };

    Ok(Some(Assignment {
        assign_to: expr1,
        value: expr2,
        typ,
        pos: merge_file_positions(&expr1_pos, &expr2_pos),
    }))
}