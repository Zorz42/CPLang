use crate::compiler::error::{CompilerError, CompilerResult};
use crate::compiler::parser::ast::{ASTExpression, ASTGlobalVariable, ASTOperator, ASTStatement, ASTStructDeclaration};
use crate::compiler::parser::expression::parse_expression;
use crate::compiler::parser::typed::parse_type_hint;
use crate::compiler::tokenizer::{Token, TokenBlock};

pub fn parse_assignment(structs: &Vec<ASTStructDeclaration>, assign_to: ASTExpression, block: &mut TokenBlock) -> CompilerResult<Option<ASTStatement>> {
    let (symbol, symbol_pos) = {
        let token = block.peek();
        match token {
            (
                Token::Assign
                | Token::PlusEquals
                | Token::MinusEquals
                | Token::MulEquals
                | Token::DivEquals
                | Token::ModEquals
                | Token::Increment
                | Token::Decrement,
                _,
            ) => block.get(),
            _ => return Ok(None),
        }
    };

    let assign_to_pos = assign_to.pos;

    let res = match &symbol {
        Token::Assign | Token::PlusEquals | Token::MinusEquals | Token::MulEquals | Token::DivEquals | Token::ModEquals => {
            let value = parse_expression(structs, block)?;
            let pos = assign_to_pos + value.pos;
            match symbol {
                Token::Assign => ASTStatement::Assignment { assign_to, value, pos },
                Token::PlusEquals => ASTStatement::AssignmentOperator {
                    assign_to,
                    value,
                    operator: ASTOperator::Plus,
                },
                Token::MinusEquals => ASTStatement::AssignmentOperator {
                    assign_to,
                    value,
                    operator: ASTOperator::Minus,
                },
                Token::MulEquals => ASTStatement::AssignmentOperator {
                    assign_to,
                    value,
                    operator: ASTOperator::Mul,
                },
                Token::DivEquals => ASTStatement::AssignmentOperator {
                    assign_to,
                    value,
                    operator: ASTOperator::Div,
                },
                Token::ModEquals => ASTStatement::AssignmentOperator {
                    assign_to,
                    value,
                    operator: ASTOperator::Mod,
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

pub fn parse_global_variable_declaration(structs: &Vec<ASTStructDeclaration>, block: &mut TokenBlock, file_idx: usize) -> CompilerResult<ASTGlobalVariable> {
    let (ident, ident_pos) = match block.get() {
        (Token::Identifier(ident), ident_pos) => (ident, ident_pos),
        (Token::End, _) => {
            return Err(CompilerError {
                message: "Expected another token after this one".to_string(),
                position: Some(block.get_last_pos()),
            });
        }
        (_, pos) => {
            return Err(CompilerError {
                message: "Unexpected token, expected one of: fn, struct, identifier".to_owned(),
                position: Some(pos),
            });
        }
    };

    let hint = parse_type_hint(block)?;

    let value = if let (Token::Assign, _) = block.peek() {
        block.get();
        let value = parse_expression(structs, block)?;
        Some(value)
    } else {
        None
    };

    Ok(ASTGlobalVariable {
        name: ident,
        type_hint: hint,
        initial_value: value,
        pos: ident_pos,
        file_idx,
    })
}
