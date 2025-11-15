use crate::compiler::error::{merge_file_positions, CompilerResult};
use crate::compiler::parser::ast::{ASTOperator, ASTStatement, ASTStructDeclaration};
use crate::compiler::parser::expression::parse_expression;
use crate::compiler::tokenizer::{Symbol, Token, TokenBlock};

pub fn parse_assignment(structs: &Vec<ASTStructDeclaration>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<Option<ASTStatement>> {
    let old_idx = *curr_idx;
    if *curr_idx + 1 >= block.children.len() {
        return Ok(None);
    }

    let assign_to = match parse_expression(structs, block, curr_idx) {
        Ok(x) => x,
        Err(_) => {
            *curr_idx = old_idx;
            return Ok(None);
        }
    };
    let assign_to_pos = assign_to.get_pos();

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
            let value = parse_expression(structs, block, curr_idx)?;
            let value_pos = value.get_pos();
            let pos = merge_file_positions(&assign_to_pos, &value_pos);
            match symbol {
                Symbol::Assign => ASTStatement::Assignment { assign_to, value, pos },
                Symbol::Increase => ASTStatement::AssignmentOperator {
                    assign_to,
                    value,
                    operator: ASTOperator::Plus,
                    pos,
                },
                Symbol::Decrease => ASTStatement::AssignmentOperator {
                    assign_to,
                    value,
                    operator: ASTOperator::Minus,
                    pos,
                },
                _ => unreachable!(),
            }
        }

        Symbol::Increment => ASTStatement::AssignmentIncrement {
            assign_to,
            pos: merge_file_positions(&assign_to_pos, &symbol_pos),
        },
        Symbol::Decrement => ASTStatement::AssignmentDecrement {
            assign_to,
            pos: merge_file_positions(&assign_to_pos, &symbol_pos),
        },
        _ => unreachable!(),
    };

    Ok(Some(res))
}
