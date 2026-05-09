use crate::compiler::error::{CompilerError, CompilerResult};
use crate::compiler::parser::ast::{ASTExpression, ASTOperator, ASTStatement, ASTStructDeclaration, ASTType};
use crate::compiler::parser::expression::parse_expression;
use crate::compiler::parser::typed::parse_type_hint;
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

pub fn parse_global_variable_declaration(structs: &Vec<ASTStructDeclaration>, block: &mut TokenBlock) -> CompilerResult<(String, ASTType, Option<ASTExpression>)> {
    let ident = match block.get() {
        (Token::Identifier(ident), _) => ident,
        (_, pos) => return Err(CompilerError {
            message: "Unexpected token, expected one of: fn, struct, identifier".to_owned(),
            position: Some(pos),
        }),
    };

    let hint = parse_type_hint(block)?;

    let value = if let (Token::Assign, _) = block.peek() {
        block.get();
        let value = parse_expression(structs, block)?;
        Some(value)
    } else {
        None
    };

    Ok((ident, hint, value))
}
