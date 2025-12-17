use crate::compiler::error::{CompilerError, CompilerResult};
use crate::compiler::parser::ast::{ASTExpression, ASTExpressionKind, ASTOperator, ASTStructDeclaration};
use crate::compiler::parser::function::parse_function_call;
use crate::compiler::parser::structure::parse_struct_instantiation;
use crate::compiler::parser::typed::parse_type_hint;
use crate::compiler::tokenizer::{Constant, Token, TokenBlock};

// only looks for a single value (if parentheses are used, it will parse whole expression)
fn parse_value(structs: &Vec<ASTStructDeclaration>, block: &mut TokenBlock) -> CompilerResult<ASTExpression> {
    // check for function call first
    if let Some((call, pos)) = parse_function_call(structs, block)? {
        return Ok(ASTExpression::no_hint(ASTExpressionKind::FunctionCall(call), pos));
    }

    let mut res = match block.get() {
        (Token::Constant(constant), pos) => match constant {
            Constant::Integer(int) => ASTExpression::no_hint(ASTExpressionKind::Integer(int), pos),
            Constant::Float(float) => ASTExpression::no_hint(ASTExpressionKind::Float(float), pos),
            Constant::String(string) => ASTExpression::no_hint(ASTExpressionKind::String(string.iter().map(|x| x.c).collect()), pos),
            Constant::Boolean(boolean) => ASTExpression::no_hint(ASTExpressionKind::Boolean(boolean), pos),
        },
        (Token::Identifier(identifier), pos) => {
            // we need to know if this is a struct instantiation or a variable
            if let Some(struct_declaration) = structs.iter().find(|x| x.name == *identifier) {
                parse_struct_instantiation(structs, block, struct_declaration, pos, identifier)?
            } else {
                ASTExpression::no_hint(ASTExpressionKind::Variable(identifier.clone()), pos)
            }
        }
        (Token::Reference, _) => {
            let res = parse_value(structs, block)?;
            let pos = res.pos;

            ASTExpression::no_hint(ASTExpressionKind::Reference(Box::new(res)), pos)
        }
        (Token::Pipe, _) => {
            let res = parse_value(structs, block)?;
            let pos = res.pos;

            ASTExpression::no_hint(ASTExpressionKind::Dereference(Box::new(res)), pos)
        }
        (Token::ParenthesisBlock(mut block), _) => parse_expression(structs, &mut block)?,
        (_, pos) => {
            return Err(CompilerError {
                message: "Unexpected token".to_owned(),
                position: Some(pos),
            });
        }
    };

    while Token::Dot == block.peek().0 {
        block.get();

        res = if let Some((call, pos)) = parse_function_call(structs, block)? {
            ASTExpression::no_hint(ASTExpressionKind::MethodCall {
                expression: Box::new(res),
                call,
            }, pos)
        } else {
            match block.get() {
                (Token::Identifier(identifier), pos) => {
                    ASTExpression::no_hint(ASTExpressionKind::FieldAccess {
                        expression: Box::new(res),
                        field_name: identifier,
                    }, pos)
                }
                (_, pos) => {
                    return Err(CompilerError {
                        message: "Expected identifier after dot".to_owned(),
                        position: Some(pos),
                    });
                }
            }
        }
    }

    res.type_hint = parse_type_hint(block)?;

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

    let operator_precedence = vec![
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
                    ASTExpression::no_hint(ASTExpressionKind::BinaryOperation {
                        expression1: Box::new(expression1),
                        operator,
                        expression2: Box::new(expression2),
                    }, pos),
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
