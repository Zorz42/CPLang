use crate::compiler::error::{merge_file_positions, CompilerResult};
use crate::compiler::parser::ast::{ASTStructDeclaration, ASTStatement};
use crate::compiler::parser::expression::{parse_expression};
use crate::compiler::tokenizer::{Symbol, Token, TokenBlock};


pub fn parse_assignment(structs: &Vec<ASTStructDeclaration>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<Option<ASTStatement>> {
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

    let (symbol, symbol_pos) = if let Some((Token::Symbol(symbol), pos)) = block.children.get(*curr_idx).cloned() {
        match symbol {
            Symbol::Assign | Symbol::Increase | Symbol::Decrease | Symbol::Increment | Symbol::Decrement => (symbol, pos),
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

    let res = match symbol.clone() {
        Symbol::Assign | Symbol::Increase | Symbol::Decrease => {
            let (expr2, expr2_pos) = parse_expression(structs, block, curr_idx)?;
            let pos = merge_file_positions(&expr1_pos, &expr2_pos);
            match symbol {
                Symbol::Assign => ASTStatement::Assignment(expr1, expr2, pos),
                Symbol::Increase => ASTStatement::AssignmentIncrease(expr1, expr2, pos),
                Symbol::Decrease => ASTStatement::AssignmentDecrease(expr1, expr2, pos),
                _ => unreachable!(),
            }
        }

        Symbol::Increment => ASTStatement::AssignmentIncrement(expr1, merge_file_positions(&expr1_pos, &symbol_pos)),
        Symbol::Decrement => ASTStatement::AssignmentDecrement(expr1, merge_file_positions(&expr1_pos, &symbol_pos)),
        _ => unreachable!(),
    };

    Ok(Some(res))
}