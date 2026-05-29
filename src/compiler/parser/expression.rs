use crate::compiler::error::{CompilerError, CompilerResult};
use crate::compiler::parser::ast::{ASTExpression, ASTExpressionKind, ASTOperator, ASTStructDeclaration, ASTType};
use crate::compiler::parser::function::parse_function_call;
use crate::compiler::parser::structure::parse_struct_instantiation;
use crate::compiler::parser::typed::parse_type_hint;
use crate::compiler::tokenizer::{Token, TokenBlock};

// only looks for a single value (if parentheses are used, it will parse whole expression)
fn parse_value(structs: &Vec<ASTStructDeclaration>, block: &mut TokenBlock) -> CompilerResult<ASTExpression> {
    // check for function call first
    if let Some((call, pos)) = parse_function_call(structs, block)? {
        return Ok(ASTExpression::new(ASTExpressionKind::FunctionCall(call), pos));
    }

    let mut res = match block.get() {
        (Token::ConstInteger(int), pos) => ASTExpression::new(ASTExpressionKind::Integer(int), pos),
        (Token::ConstFloat(float), pos) => ASTExpression::new(ASTExpressionKind::Float(float), pos),
        (Token::ConstString(string), pos) => ASTExpression::new(ASTExpressionKind::String(string.iter().map(|x| x.c).collect()), pos),
        (Token::ConstBoolean(boolean), pos) => ASTExpression::new(ASTExpressionKind::Boolean(boolean), pos),

        (Token::Identifier(identifier), pos) => {
            // we need to know if this is a struct instantiation or a variable
            if let Some(struct_declaration) = structs.iter().find(|x| x.name == *identifier) {
                parse_struct_instantiation(structs, block, struct_declaration, pos, identifier)?
            } else {
                ASTExpression::new(ASTExpressionKind::Variable(identifier.clone()), pos)
            }
        }
        (Token::Reference, _) => {
            let res = parse_value(structs, block)?;
            let pos = res.pos;

            ASTExpression::new(ASTExpressionKind::Reference(Box::new(res)), pos)
        }
        (Token::Pipe, _) => {
            let res = parse_value(structs, block)?;
            let pos = res.pos;

            ASTExpression::new(ASTExpressionKind::Dereference(Box::new(res)), pos)
        }
        (Token::ParenthesisBlock(mut block), _) => parse_expression(structs, &mut block)?,
        (_, pos) => {
            return Err(CompilerError {
                message: "Unexpected token".to_owned(),
                position: Some(pos),
            });
        }
    };

    // parse all field accesses and indexes
    loop {
        match block.peek().0 {
            // field access
            Token::Dot => {
                block.get();

                res = if let Some((call, pos)) = parse_function_call(structs, block)? {
                    ASTExpression::new(
                        ASTExpressionKind::MethodCall {
                            expression: Box::new(res),
                            call,
                        },
                        pos,
                    )
                } else {
                    match block.get() {
                        (Token::Identifier(identifier), pos) => ASTExpression::new(
                            ASTExpressionKind::FieldAccess {
                                expression: Box::new(res),
                                field_name: identifier,
                            },
                            pos,
                        ),
                        (Token::ConstInteger(index), pos) if index >= 0 => ASTExpression::new(
                            ASTExpressionKind::TupleAccess {
                                expression: Box::new(res),
                                field_index: index as usize,
                            },
                            pos,
                        ),
                        (_, pos) => {
                            return Err(CompilerError {
                                message: "Expected identifier or non-negative integer after dot".to_owned(),
                                position: Some(pos),
                            });
                        }
                    }
                }
            }
            // index
            Token::BracketBlock(_) => {
                let pos = res.pos + block.peek().1;
                let Token::BracketBlock(mut block) = block.get().0 else { unreachable!() };
                let mut arguments = Vec::new();

                while block.has_tokens() {
                    arguments.push(parse_expression(structs, &mut block)?);
                }

                res = ASTExpression::new(
                    ASTExpressionKind::Index {
                        expression: Box::new(res),
                        arguments,
                    },
                    pos,
                );
            }
            _ => break,
        }
    }

    let type_hint = parse_type_hint(block)?;
    if !matches!(type_hint, ASTType::Any(_)) {
        let pos = res.pos + type_hint.get_pos();
        res = ASTExpression::new(
            ASTExpressionKind::TypeHint {
                expression: Box::new(res),
                type_hint,
            },
            pos,
        );
    }

    Ok(res)
}

const fn token_to_operator(symbol: &Token) -> Option<ASTOperator> {
    match symbol {
        Token::Plus => Some(ASTOperator::Plus),
        Token::Star => Some(ASTOperator::Mul),
        Token::Slash => Some(ASTOperator::Div),
        Token::Equals => Some(ASTOperator::Equals),
        Token::GreaterThan => Some(ASTOperator::Greater),
        Token::LessThan => Some(ASTOperator::Lesser),
        Token::GreaterThanOrEqual => Some(ASTOperator::GreaterEq),
        Token::LessThanOrEqual => Some(ASTOperator::LesserEq),
        Token::Minus => Some(ASTOperator::Minus),
        Token::NotEquals => Some(ASTOperator::NotEquals),
        Token::Comma => Some(ASTOperator::Comma),
        Token::DotDot => Some(ASTOperator::DotDot),
        _ => None,
    }
}

// looks for operators and values
pub fn parse_expression(structs: &Vec<ASTStructDeclaration>, block: &mut TokenBlock) -> CompilerResult<ASTExpression> {
    let mut vals = Vec::new();
    let mut ops = Vec::new();
    let first_val = parse_value(structs, block)?;
    vals.push(first_val);
    while let Some(op) = token_to_operator(&block.peek().0) {
        block.get();
        ops.push(op);
        vals.push(parse_value(structs, block)?);
    }

    let operator_precedence = [
        vec![ASTOperator::Mul, ASTOperator::Div],
        vec![ASTOperator::Plus, ASTOperator::Minus],
        vec![
            ASTOperator::Equals,
            ASTOperator::Greater,
            ASTOperator::Lesser,
            ASTOperator::GreaterEq,
            ASTOperator::LesserEq,
            ASTOperator::NotEquals,
        ],
        vec![ASTOperator::Comma],
        vec![ASTOperator::DotDot],
    ];

    for operators in operator_precedence {
        // merge values with operators
        loop {
            let mut idx = None;
            for (i, op) in ops.iter().enumerate() {
                if operators.contains(op) {
                    idx = Some(i);
                    break;
                }
            }

            if let Some(i) = idx {
                let expression1 = vals.remove(i);
                let expression2 = vals.remove(i);
                let operator = ops.remove(i);
                let pos = expression1.pos + expression2.pos;
                vals.insert(
                    i,
                    ASTExpression::new(
                        ASTExpressionKind::BinaryOperation {
                            expression1: Box::new(expression1),
                            operator,
                            expression2: Box::new(expression2),
                        },
                        pos,
                    ),
                );
            } else {
                break;
            }
        }
    }

    assert_eq!(vals.len(), 1);
    assert_eq!(ops.len(), 0);

    Ok(vals.pop().unwrap())
}
