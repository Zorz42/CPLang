use crate::compiler::error::{merge_file_positions, CompilerError, CompilerResult};
use crate::compiler::parser::ast::{ASTExpression, ASTOperator, ASTStructDeclaration};
use crate::compiler::tokenizer::{Constant, Token, TokenBlock};
use std::collections::HashMap;

// only looks for a single value (if parentheses are used, it will parse whole expression)
fn parse_value(structs: &Vec<ASTStructDeclaration>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<ASTExpression> {
    let mut pos = block.children[*curr_idx].1.clone();
    let mut res = match &block.children[*curr_idx].0 {
        Token::Constant(constant) => {
            *curr_idx += 1;
            match constant {
                Constant::Integer(int) => ASTExpression::Integer(*int, pos),
                Constant::Float(float) => ASTExpression::Float(*float, pos),
                Constant::String(string) => ASTExpression::String(string.iter().map(|x| x.c).collect(), pos),
                Constant::Boolean(boolean) => ASTExpression::Boolean(*boolean, pos),
            }
        }
        Token::Identifier(identifier) => {
            *curr_idx += 1;

            // we need to know if this is a function call, a struct instantiation or a variable
            if let Some((Token::ParenthesisBlock(call_block), call_block_pos)) = block.children.get(*curr_idx) {
                let mut args = Vec::new();
                let mut block_idx = 0;
                *curr_idx += 1;
                while block_idx < call_block.children.len() {
                    let expr = parse_expression(structs, call_block, &mut block_idx)?;
                    args.push(expr);
                }
                ASTExpression::FunctionCall {
                    name: identifier.clone(),
                    arguments: args,
                    pos: merge_file_positions(&pos, call_block_pos),
                }
            } else if let Some(struct_declaration) = structs.iter().find(|x| x.name == *identifier) {
                let mut fields = HashMap::new();
                let mut fields_left = struct_declaration.fields.len();

                while fields_left > 0 {
                    let (field_name, field_pos) = match &block.children.get(*curr_idx).map(|x| x.0.clone()) {
                        Some(Token::Identifier(ident)) => {
                            if !struct_declaration.fields.iter().any(|(name, _type_hint)| name == ident) {
                                return Err(CompilerError {
                                    message: format!("Field with name {ident} not found"),
                                    position: Some(block.children[*curr_idx].1.clone()),
                                });
                            }

                            (ident.clone(), block.children[*curr_idx].1.clone())
                        }
                        _ => {
                            return Err(CompilerError {
                                message: "Expected struct field identifier after this token".to_owned(),
                                position: Some(block.children[*curr_idx - 1].1.clone()),
                            });
                        }
                    };

                    *curr_idx += 1;

                    let expr = parse_expression(structs, block, curr_idx)?;
                    let expr_pos = expr.get_pos();

                    if fields.contains_key(&field_name) {
                        return Err(CompilerError {
                            message: format!("Field {} assigned twice.", field_name),
                            position: Some(field_pos),
                        });
                    }

                    fields.insert(field_name, expr);
                    pos = merge_file_positions(&pos, &expr_pos);
                    fields_left -= 1;
                }

                let mut fields_res = Vec::new();
                for (field_name, _field_type) in struct_declaration.fields.iter() {
                    fields_res.push(fields[field_name].clone());
                }

                ASTExpression::StructInitialization {
                    name: identifier.clone(),
                    fields: fields_res,
                    pos,
                }
            } else {
                ASTExpression::Variable(identifier.clone(), pos.clone())
            }
        }
        Token::Reference => {
            *curr_idx += 1;
            let res = parse_value(structs, block, curr_idx)?;
            let pos = res.get_pos();

            ASTExpression::Reference {
                expression: Box::new(res),
                pos: pos.clone(),
            }
        }
        Token::Colon => {
            *curr_idx += 1;
            let res = parse_value(structs, block, curr_idx)?;
            let pos = res.get_pos();

            ASTExpression::Dereference {
                expression: Box::new(res),
                pos: pos.clone(),
            }
        }
        Token::ParenthesisBlock(block) => {
            *curr_idx += 1;
            parse_expression(structs, block, &mut 0)?
        }
        _ => {
            let pos = &block.children[*curr_idx].1;
            return Err(CompilerError {
                message: "Unexpected token".to_owned(),
                position: Some(pos.clone()),
            });
        }
    };

    while let Some((Token::Dot, _)) = block.children.get(*curr_idx) {
        *curr_idx += 1;
        match block.children.get(*curr_idx) {
            Some((Token::Identifier(s), pos)) => {
                *curr_idx += 1;
                if let Some(Token::ParenthesisBlock(call_block)) = block.children.get(*curr_idx).map(|x| &x.0) {
                    let mut args = Vec::new();
                    let mut block_idx = 0;
                    *curr_idx += 1;
                    while block_idx < call_block.children.len() {
                        let expr = parse_expression(structs, call_block, &mut block_idx)?;
                        args.push(expr);
                    }
                    res = ASTExpression::MethodCall {
                        expression: Box::new(res),
                        pos: pos.clone(),
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
            _ => {
                return Err(CompilerError {
                    message: "Expected identifier after dot".to_owned(),
                    position: Some(block.children[*curr_idx - 1].1.clone()),
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
pub fn parse_expression(structs: &Vec<ASTStructDeclaration>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<ASTExpression> {
    let mut vals = Vec::new();
    let mut ops = Vec::new();
    let first_val = parse_value(structs, block, curr_idx)?;
    vals.push(first_val);
    while *curr_idx < block.children.len() {
        if let Some(op) = token_to_operator(&block.children[*curr_idx].0) {
            *curr_idx += 1;
            ops.push(op);
            vals.push(parse_value(structs, block, curr_idx)?);
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
                let pos = merge_file_positions(&expression1.get_pos(), &expression2.get_pos());
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
