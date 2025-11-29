use crate::compiler::error::{merge_file_positions, CompilerResult};
use crate::compiler::parser::ast::{ASTOperator, ASTStatement, ASTStructDeclaration};
use crate::compiler::parser::expression::parse_expression;
use crate::compiler::tokenizer::{Token, TokenBlock};

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

    let (symbol, symbol_pos) = {
        let token = block.children.get(*curr_idx).cloned();
        match &token {
            Some((Token::Assign, _)) |
            Some((Token::Increase, _)) |
            Some((Token::Decrease, _)) |
            Some((Token::Increment, _)) |
            Some((Token::Decrement, _)) => token.unwrap(),
            _ => {
                *curr_idx = old_idx;
                return Ok(None);
            }
        }
    };

    *curr_idx += 1;

    let res = match symbol.clone() {
        Token::Assign | Token::Increase | Token::Decrease => {
            let value = parse_expression(structs, block, curr_idx)?;
            let value_pos = value.get_pos();
            let pos = merge_file_positions(&assign_to_pos, &value_pos);
            match symbol {
                Token::Assign => ASTStatement::Assignment { assign_to, value, pos },
                Token::Increase => ASTStatement::AssignmentOperator {
                    assign_to,
                    value,
                    operator: ASTOperator::Plus,
                    pos,
                },
                Token::Decrease => ASTStatement::AssignmentOperator {
                    assign_to,
                    value,
                    operator: ASTOperator::Minus,
                    pos,
                },
                _ => unreachable!(),
            }
        }

        Token::Increment => ASTStatement::AssignmentIncrement {
            assign_to,
            pos: merge_file_positions(&assign_to_pos, &symbol_pos),
        },
        Token::Decrement => ASTStatement::AssignmentDecrement {
            assign_to,
            pos: merge_file_positions(&assign_to_pos, &symbol_pos),
        },
        _ => unreachable!(),
    };

    Ok(Some(res))
}
