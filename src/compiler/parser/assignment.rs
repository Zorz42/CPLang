use crate::compiler::error::{merge_file_positions, CompilerResult};
use crate::compiler::parser::ast::{ASTExpression, ASTOperator, ASTStatement, ASTStructDeclaration};
use crate::compiler::parser::expression::parse_expression;
use crate::compiler::tokenizer::{Token, TokenBlock};

pub fn parse_assignment(structs: &Vec<ASTStructDeclaration>, assign_to: &ASTExpression, block: &mut TokenBlock) -> CompilerResult<Option<ASTStatement>> {
    let (symbol, symbol_pos) = {
        let token = block.peek();
        match token {
            (Token::Assign, _) |
            (Token::Increase, _) |
            (Token::Decrease, _) |
            (Token::Increment, _) |
            (Token::Decrement, _) => block.get(),
            _ => {
                return Ok(None);
            }
        }
    };

    let assign_to_pos = assign_to.get_pos();
    let assign_to = assign_to.clone();

    let res = match symbol.clone() {
        Token::Assign | Token::Increase | Token::Decrease => {
            let value = parse_expression(structs, block)?;
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
