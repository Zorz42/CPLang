use crate::compiler::error::{CompilerError, CompilerResult, FilePosition, merge_file_positions};
use crate::compiler::parser::ast::{ASTExpression, ASTOperator, ASTStructDeclaration};
use crate::compiler::tokenizer::{Constant, Symbol, Token, TokenBlock};
use std::collections::HashMap;

// only looks for a single value (if parentheses are used, it will parse whole expression)
fn parse_value(structs: &Vec<ASTStructDeclaration>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<(ASTExpression, FilePosition)> {
    let mut pos = block.children[*curr_idx].1.clone();
    let mut res = match &block.children[*curr_idx].0 {
        Token::Constant(constant) => {
            *curr_idx += 1;
            let expr = match constant {
                Constant::Integer(int) => ASTExpression::Integer(*int),
                Constant::Float(float) => ASTExpression::Float(*float),
                Constant::String(string) => ASTExpression::String(string.iter().map(|x| x.c).collect()),
                Constant::Boolean(boolean) => ASTExpression::Boolean(*boolean),
            };

            (expr, pos)
        }
        Token::Identifier(identifier) => {
            *curr_idx += 1;

            // we need to know if this is a function call, a struct instantiation or a variable
            if let Some(Token::ParenthesisBlock(call_block)) = block.children.get(*curr_idx).map(|x| x.0.clone()) {
                let mut args = Vec::new();
                let mut block_idx = 0;
                *curr_idx += 1;
                while block_idx < call_block.children.len() {
                    let (expr, expr_pos) = parse_expression(structs, &call_block, &mut block_idx)?;
                    args.push(expr);
                    pos = merge_file_positions(&pos, &expr_pos);
                }
                (
                    ASTExpression::FunctionCall {
                        name: identifier.clone(),
                        arguments: args,
                    },
                    pos,
                )
            } else if let Some(struct_declaration) = structs.iter().find(|x| x.name == *identifier) {
                let mut fields = HashMap::new();
                let mut fields_left = struct_declaration.fields.len();

                while fields_left > 0 {
                    let (field_name, field_pos) = match &block.children.get(*curr_idx).map(|x| x.0.clone()) {
                        Some(Token::Identifier(ident)) => (ident.clone(), block.children[*curr_idx].1.clone()),
                        _ => {
                            return Err(CompilerError {
                                message: "Expected struct field identifier after this token".to_owned(),
                                position: Some(block.children[*curr_idx - 1].1.clone()),
                            });
                        }
                    };

                    *curr_idx += 1;

                    let (expr, expr_pos) = parse_expression(structs, block, curr_idx)?;

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
                for field in struct_declaration.fields.iter() {
                    fields_res.push(fields[field].clone());
                }

                (
                    ASTExpression::StructInitialization {
                        name: identifier.clone(),
                        fields: fields_res,
                    },
                    pos,
                )
            } else {
                (ASTExpression::Variable(identifier.clone(), pos.clone()), pos)
            }
        }
        Token::Symbol(Symbol::Reference) => {
            *curr_idx += 1;
            let (res, pos) = parse_value(structs, block, curr_idx)?;

            (
                ASTExpression::Reference {
                    expression: Box::new(res),
                    pos: pos.clone(),
                },
                pos,
            )
        }
        Token::Symbol(Symbol::Colon) => {
            *curr_idx += 1;
            let (res, pos) = parse_value(structs, block, curr_idx)?;

            (
                ASTExpression::Dereference {
                    expression: Box::new(res),
                    pos: pos.clone(),
                },
                pos,
            )
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

    while let Some((Token::Symbol(Symbol::Dot), _)) = block.children.get(*curr_idx) {
        *curr_idx += 1;
        match block.children.get(*curr_idx) {
            Some((Token::Identifier(s), pos)) => {
                *curr_idx += 1;
                if let Some(Token::ParenthesisBlock(call_block)) = block.children.get(*curr_idx).map(|x| &x.0) {
                    let mut args = Vec::new();
                    let mut block_idx = 0;
                    *curr_idx += 1;
                    while block_idx < call_block.children.len() {
                        let (expr, _) = parse_expression(structs, call_block, &mut block_idx)?;
                        args.push(expr);
                    }
                    res.0 = ASTExpression::MethodCall {
                        expression: Box::new(res.0),
                        pos: pos.clone(),
                        method_name: s.clone(),
                        arguments: args,
                    };
                } else {
                    res.0 = ASTExpression::FieldAccess {
                        expression: Box::new(res.0),
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

fn symbol_to_operator(symbol: &Symbol) -> Option<ASTOperator> {
    match symbol {
        Symbol::Plus => Some(ASTOperator::Plus),
        Symbol::Star => Some(ASTOperator::Mul),
        Symbol::Slash => Some(ASTOperator::Div),
        Symbol::Equals => Some(ASTOperator::Equals),
        Symbol::GreaterThan => Some(ASTOperator::Greater),
        Symbol::LessThan => Some(ASTOperator::Less),
        Symbol::GreaterThanOrEqual => Some(ASTOperator::GreaterEquals),
        Symbol::LessThanOrEqual => Some(ASTOperator::LessEquals),
        Symbol::Minus => Some(ASTOperator::Minus),
        Symbol::NotEquals => Some(ASTOperator::NotEquals),
        _ => None,
    }
}

// looks for operators and values
pub fn parse_expression(structs: &Vec<ASTStructDeclaration>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<(ASTExpression, FilePosition)> {
    let mut vals = Vec::new();
    let mut ops = Vec::new();
    let (first_val, first_pos) = parse_value(structs, block, curr_idx)?;
    vals.push((first_val, first_pos));
    while *curr_idx < block.children.len() {
        match &block.children[*curr_idx].0 {
            Token::Symbol(symbol) => {
                if let Some(op) = symbol_to_operator(symbol) {
                    *curr_idx += 1;
                    ops.push(op);
                } else {
                    break;
                }

                vals.push(parse_value(structs, block, curr_idx)?);
            }
            _ => break,
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
                let (expression1, expression1_pos) = vals.remove(i);
                let (expression2, expression2_pos) = vals.remove(i);
                let operator = ops.remove(i);
                let pos = merge_file_positions(&expression1_pos, &expression2_pos);
                vals.insert(
                    i,
                    (
                        ASTExpression::BinaryOperation {
                            expression1: Box::new(expression1),
                            operator,
                            expression2: Box::new(expression2),
                            pos: pos.clone(),
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
