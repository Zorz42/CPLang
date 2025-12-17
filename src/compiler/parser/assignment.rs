use crate::compiler::error::CompilerResult;
use crate::compiler::parser::ast::{ASTExpression, ASTOperator, ASTStatement, ASTStructDeclaration};
use crate::compiler::parser::expression::parse_expression;
use crate::compiler::tokenizer::{Token, TokenBlock};

pub fn parse_assignment(structs: &Vec<ASTStructDeclaration>, assign_to: ASTExpression, block: &mut TokenBlock) -> CompilerResult<Option<ASTStatement>> {
    let (symbol, symbol_pos) = {
        let token = block.peek();
        match token {
            (Token::Assign | Token::Increase | Token::Decrease | Token::Increment | Token::Decrement, _) => block.get(),
            _ => return Ok(None),
        }
    };

    let assign_to_pos = assign_to.pos;

    let res = match &symbol {
        Token::Assign | Token::Increase | Token::Decrease => {
            let value = parse_expression(structs, block)?;
            let pos = assign_to_pos + value.pos;
            match symbol {
                Token::Assign => ASTStatement::Assignment { assign_to, value, pos },
                Token::Increase => ASTStatement::AssignmentOperator {
                    assign_to,
                    value,
                    operator: ASTOperator::Plus,
                },
                Token::Decrease => ASTStatement::AssignmentOperator {
                    assign_to,
                    value,
                    operator: ASTOperator::Minus,
                },
                _ => unreachable!(),
            }
        }

        Token::Increment => ASTStatement::AssignmentIncrement {
            assign_to,
            pos: assign_to_pos + symbol_pos,
        },
        Token::Decrement => ASTStatement::AssignmentDecrement {
            assign_to,
            pos: assign_to_pos + symbol_pos,
        },
        _ => unreachable!(),
    };

    Ok(Some(res))
}
