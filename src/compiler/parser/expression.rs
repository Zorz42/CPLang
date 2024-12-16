use std::collections::HashMap;
use crate::compiler::error::{merge_file_positions, CompilerError, CompilerResult, FilePosition};
use crate::compiler::parser::function::FunctionSignature;
use crate::compiler::parser::structure::StructDeclaration;
use crate::compiler::tokenizer::{TokenBlock, Constant, Token, Symbol};

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Operator {
    Plus,
    Mul,
    Equals,
    Greater,
    Less,
    GreaterEquals,
    LessEquals,
    Minus,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Integer(i32),
    Float(f32),
    String(String),
    Boolean(bool),
    Variable(String, FilePosition),
    Reference(String, FilePosition),
    FunctionCall(String, Vec<Expression>),
    StructInitialization(String, Vec<Expression>),
    FieldAccess(Box<Expression>, String),
    BinaryOperation(Box<Expression>, Operator, Box<Expression>, FilePosition),
}

// only looks for a single value (if parentheses are used, it will parse whole expression)
fn parse_value(functions: &Vec<FunctionSignature>, structs: &Vec<StructDeclaration>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<(Expression, FilePosition)> {
    let mut pos = block.children[*curr_idx].1.clone();
    let mut res = match &block.children[*curr_idx].0 {
        Token::Constant(Constant::Integer(int)) => {
            *curr_idx += 1;
            (Expression::Integer(*int), pos)
        },
        Token::Constant(Constant::Float(float)) => {
            *curr_idx += 1;
            (Expression::Float(*float), pos)
        },
        Token::Constant(Constant::String(string)) => {
            *curr_idx += 1;
            (Expression::String(string.clone()), pos)
        },
        Token::Constant(Constant::Boolean(boolean)) => {
            *curr_idx += 1;
            (Expression::Boolean(*boolean), pos)
        },
        Token::Identifier(identifier) => {
            *curr_idx += 1;

            // we need to know if this is a function call, a struct instantiation or a variable
            if let Some(function) = functions.iter().find(|f| f.name == *identifier) {
                let num_args = function.args.len();
                let mut args = Vec::new();
                for _ in 0..num_args {
                    let (expr, expr_pos) = parse_expression(functions, structs, block, curr_idx)?;
                    args.push(expr);
                    pos = merge_file_positions(&pos, &expr_pos);
                }
                (Expression::FunctionCall(identifier.clone(), args), pos)
            } else if let Some(struct_declaration) = structs.iter().find(|x| x.name == *identifier) {
                let mut fields = HashMap::new();
                let mut fields_left = struct_declaration.fields.len();

                while fields_left > 0 {
                    let (field_name, field_pos) = match &block.children.get(*curr_idx).map(|x| x.0.clone()) {
                        Some(Token::Identifier(ident)) => (ident.clone(), block.children[*curr_idx].1.clone()),
                        _ => {
                            return Err(CompilerError {
                                message: "Expected struct field identifier after this token".to_owned(),
                                position: Some(block.children[*curr_idx - 1].1.clone())
                            });
                        },
                    };

                    *curr_idx += 1;

                    let (expr, expr_pos) = parse_expression(functions, structs, block, curr_idx)?;

                    if fields.contains_key(&field_name) {
                        return Err(CompilerError{
                            message: format!("Field {} assigned twice.", field_name),
                            position: Some(field_pos),
                        })
                    }

                    fields.insert(field_name, expr);
                    pos = merge_file_positions(&pos, &expr_pos);
                    fields_left -= 1;
                }

                let mut fields_res = Vec::new();
                for field in struct_declaration.fields.iter() {
                    if let Some(expr) = fields.get(field) {
                        fields_res.push(expr.clone());
                    } else {
                        return Err(CompilerError{
                            message: format!("Field {} not assigned.", field),
                            position: Some(pos),
                        })
                    }
                }

                (Expression::StructInitialization(identifier.clone(), fields_res), pos)
            } else {
                (Expression::Variable(identifier.clone(), pos.clone()), pos)
            }
        },
        Token::Symbol(Symbol::Reference) => {
            *curr_idx += 1;
            match &block.children[*curr_idx].0 {
                Token::Identifier(identifier) => {
                    *curr_idx += 1;
                    (Expression::Reference(identifier.clone(), pos.clone()), pos)
                },
                _ => {
                    let pos = &block.children[*curr_idx].1;
                    return Err(CompilerError {
                        message: "Expected identifier after reference symbol".to_owned(),
                        position: Some(pos.clone())
                    });
                },
            }
        }
        Token::Symbol(Symbol::LeftBracket) => {
            *curr_idx += 1;
            let res = parse_expression(functions, structs, block, curr_idx)?;
            match &block.children[*curr_idx].0 {
                Token::Symbol(Symbol::RightBracket) => {
                    *curr_idx += 1;
                    res
                },
                _ => {
                    let pos = &block.children[*curr_idx - 1].1;
                    return Err(CompilerError {
                        message: "Expected right bracket after".to_owned(),
                        position: Some(pos.clone())
                    });
                },
            }
        },
        _ => {
            let pos = &block.children[*curr_idx].1;
            return Err(CompilerError {
                message: "Unexpected token".to_owned(),
                position: Some(pos.clone())
            });
        },
    };

    while let Some((Token::Symbol(Symbol::Dot), _)) = block.children.get(*curr_idx) {
        *curr_idx += 1;
        match block.children.get(*curr_idx) {
            Some((Token::Identifier(s), _)) => {
                res.0 = Expression::FieldAccess(Box::new(res.0), s.clone());
                *curr_idx += 1;
            }
            _ => {
                return Err(CompilerError {
                    message: "Expected identifier after dot".to_owned(),
                    position: Some(block.children[*curr_idx - 1].1.clone())
                });
            }
        }
    }

    Ok(res)
}

fn symbol_to_operator(symbol: &Symbol) -> Option<Operator> {
    match symbol {
        Symbol::Plus => Some(Operator::Plus),
        Symbol::Star => Some(Operator::Mul),
        Symbol::Equals => Some(Operator::Equals),
        Symbol::GreaterThan => Some(Operator::Greater),
        Symbol::LessThan => Some(Operator::Less),
        Symbol::GreaterThanOrEqual => Some(Operator::GreaterEquals),
        Symbol::LessThanOrEqual => Some(Operator::LessEquals),
        Symbol::Minus => Some(Operator::Minus),
        _ => None,
    }
}

// looks for operators and values
pub fn parse_expression(functions: &Vec<FunctionSignature>, structs: &Vec<StructDeclaration>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<(Expression, FilePosition)> {
    let mut vals = Vec::new();
    let mut ops = Vec::new();
    let (first_val, first_pos) = parse_value(functions, structs, block, curr_idx)?;
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

                vals.push(parse_value(functions, structs, block, curr_idx)?);
            },
            _ => break,
        }
    }

    let operator_precedence = vec![
        vec![Operator::Mul],
        vec![Operator::Plus, Operator::Minus],
        vec![Operator::Equals, Operator::Greater, Operator::Less, Operator::GreaterEquals, Operator::LessEquals]
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
                let (left, left_pos) = vals.remove(i);
                let (right, right_pos) = vals.remove(i);
                let op = ops.remove(i);
                let pos = merge_file_positions(&left_pos, &right_pos);
                vals.insert(i, (Expression::BinaryOperation(Box::new(left), op, Box::new(right), pos.clone()), pos));
            } else {
                break;
            }
        }
    }

    assert_eq!(vals.len(), 1);
    assert_eq!(ops.len(), 0);

    Ok(vals.pop().unwrap())
}