use crate::compiler::error::{merge_file_positions, CompilerError, CompilerResult};
use crate::compiler::parser::ast::{ASTExpression, ASTOperator, ASTStructDeclaration};
use crate::compiler::parser::structure::parse_struct_instantiation;
use crate::compiler::tokenizer::{Constant, Token, TokenBlock};

// only looks for a single value (if parentheses are used, it will parse whole expression)
fn parse_value(structs: &Vec<ASTStructDeclaration>, block: &mut TokenBlock) -> CompilerResult<ASTExpression> {
    let mut res = match block.get() {
        (Token::Constant(constant), pos) => {
            match constant {
                Constant::Integer(int) => ASTExpression::Integer(int, pos),
                Constant::Float(float) => ASTExpression::Float(float, pos),
                Constant::String(string) => ASTExpression::String(string.iter().map(|x| x.c).collect(), pos),
                Constant::Boolean(boolean) => ASTExpression::Boolean(boolean, pos),
            }
        }
        (Token::Identifier(identifier), pos) => {

            // we need to know if this is a function call, a struct instantiation or a variable
            if let Token::ParenthesisBlock(_) = block.peek().0 {
                let (Token::ParenthesisBlock(mut call_block), call_block_pos) = block.get() else { unreachable!() };

                let mut args = Vec::new();

                while call_block.has_tokens() {
                    let expr = parse_expression(structs, &mut call_block)?;
                    args.push(expr);
                }

                ASTExpression::FunctionCall {
                    name: identifier.clone(),
                    arguments: args,
                    pos: merge_file_positions(pos, call_block_pos),
                }
            } else if let Some(struct_declaration) = structs.iter().find(|x| x.name == *identifier) {
                parse_struct_instantiation(structs, block, struct_declaration, pos, identifier)?
            } else {
                ASTExpression::Variable(identifier.clone(), pos.clone())
            }
        }
        (Token::Reference, _) => {
            let res = parse_value(structs, block)?;
            let pos = res.get_pos();

            ASTExpression::Reference {
                expression: Box::new(res),
                pos: pos.clone(),
            }
        }
        (Token::Colon, _) => {
            let res = parse_value(structs, block)?;
            let pos = res.get_pos();

            ASTExpression::Dereference {
                expression: Box::new(res),
                pos: pos.clone(),
            }
        }
        (Token::ParenthesisBlock(mut block), _) => {
            parse_expression(structs, &mut block)?
        }
        (_, pos) => {
            return Err(CompilerError {
                message: "Unexpected token".to_owned(),
                position: Some(pos.clone()),
            });
        }
    };

    while Token::Dot == block.peek().0 {
        block.get();
        match block.get() {
            (Token::Identifier(s), pos) => {
                if let Token::ParenthesisBlock(_) = block.peek().0 {
                    let mut args = Vec::new();

                    let (Token::ParenthesisBlock(mut call_block), block_pos) = block.get() else { unreachable!() };

                    while call_block.has_tokens() {
                        let expr = parse_expression(structs, &mut call_block)?;
                        args.push(expr);
                    }
                    res = ASTExpression::MethodCall {
                        expression: Box::new(res),
                        pos: merge_file_positions(pos, block_pos),
                        method_name: s.clone(),
                        arguments: args,
                    };
                } else {
                    res = ASTExpression::FieldAccess {
                        expression: Box::new(res),
                        field_name: s.clone(),
                        pos: pos.clone(),
                    };
                }
            }
            (_, pos) => {
                return Err(CompilerError {
                    message: "Expected identifier after dot".to_owned(),
                    position: Some(pos),
                });
            }
        }
    }

    Ok(res)
}

fn token_to_operator(symbol: &Token) -> Option<ASTOperator> {
    match symbol {
        Token::Plus => Some(ASTOperator::Plus),
        Token::Star => Some(ASTOperator::Mul),
        Token::Slash => Some(ASTOperator::Div),
        Token::Equals => Some(ASTOperator::Equals),
        Token::GreaterThan => Some(ASTOperator::Greater),
        Token::LessThan => Some(ASTOperator::Less),
        Token::GreaterThanOrEqual => Some(ASTOperator::GreaterEquals),
        Token::LessThanOrEqual => Some(ASTOperator::LessEquals),
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
    loop {
        if let Some(op) = token_to_operator(&block.peek().0) {
            block.get();
            ops.push(op);
            vals.push(parse_value(structs, block)?);
        } else {
            break;
        }
    }

    let operator_precedence = vec![
        vec![ASTOperator::Mul, ASTOperator::Div],
        vec![ASTOperator::Plus, ASTOperator::Minus],
        vec![
            ASTOperator::Equals,
            ASTOperator::Greater,
            ASTOperator::Less,
            ASTOperator::GreaterEquals,
            ASTOperator::LessEquals,
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
                let pos = merge_file_positions(expression1.get_pos(), expression2.get_pos());
                vals.insert(
                    i,
                    ASTExpression::BinaryOperation {
                        expression1: Box::new(expression1),
                        operator,
                        expression2: Box::new(expression2),
                        pos,
                    },
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
