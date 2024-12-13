use crate::compiler::error::{merge_file_positions, CompilerError, CompilerResult, FilePosition};
use crate::compiler::parser::function::FunctionSignature;
use crate::compiler::tokenizer::{TokenBlock, Constant, Token, Symbol};

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Operator {
    Plus,
    Mul,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Integer(i32),
    Float(f32),
    String(String),
    Boolean(bool),
    Variable(String),
    FunctionCall(String, Vec<Expression>),
    BinaryOperation(Box<Expression>, Operator, Box<Expression>),
}

// only looks for a single value (if parentheses are used, it will parse whole expression)
fn parse_value(functions: &Vec<FunctionSignature>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<(Expression, FilePosition)> {
    let mut pos = block.children[*curr_idx].1.clone();
    match &block.children[*curr_idx].0 {
        Token::Constant(Constant::Integer(int)) => {
            *curr_idx += 1;
            Ok((Expression::Integer(*int), pos))
        },
        Token::Constant(Constant::Float(float)) => {
            *curr_idx += 1;
            Ok((Expression::Float(*float), pos))
        },
        Token::Constant(Constant::String(string)) => {
            *curr_idx += 1;
            Ok((Expression::String(string.clone()), pos))
        },
        Token::Constant(Constant::Boolean(boolean)) => {
            *curr_idx += 1;
            Ok((Expression::Boolean(*boolean), pos))
        },
        Token::Identifier(identifier) => {
            *curr_idx += 1;

            // we need to know if this is a function call or a variable
            if let Some(function) = functions.iter().find(|f| f.name == *identifier) {
                let num_args = function.args.len();
                let mut args = Vec::new();
                for _ in 0..num_args {
                    let (expr, expr_pos) = parse_expression(functions, block, curr_idx)?;
                    args.push(expr);
                    pos = merge_file_positions(&pos, &expr_pos);
                }
                Ok((Expression::FunctionCall(identifier.clone(), args), pos))
            } else {
                Ok((Expression::Variable(identifier.clone()), pos))
            }
        },
        Token::Symbol(Symbol::LeftBracket) => {
            *curr_idx += 1;
            let res = parse_expression(functions, block, curr_idx)?;
            match &block.children[*curr_idx].0 {
                Token::Symbol(Symbol::RightBracket) => {
                    *curr_idx += 1;
                    Ok(res)
                },
                _ => {
                    let pos = &block.children[*curr_idx - 1].1;
                    Err(CompilerError {
                        message: "Expected right bracket after".to_owned(),
                        position: Some(pos.clone())
                    })
                },
            }
        },
        _ => {
            let pos = &block.children[*curr_idx - 1].1;
            Err(CompilerError {
                message: "Expected value after".to_owned(),
                position: Some(pos.clone())
            })
        },
    }
}

// looks for operators and values
pub fn parse_expression(functions: &Vec<FunctionSignature>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<(Expression, FilePosition)> {
    let mut vals = Vec::new();
    let mut ops = Vec::new();
    let (first_val, mut pos) = parse_value(functions, block, curr_idx)?;
    vals.push(first_val);
    while *curr_idx < block.children.len() {
        match &block.children[*curr_idx].0 {
            Token::Symbol(symbol) => {
                match symbol {
                    Symbol::Plus => {
                        *curr_idx += 1;
                        ops.push(Operator::Plus);
                    },
                    Symbol::Star => {
                        *curr_idx += 1;
                        ops.push(Operator::Mul);
                    },
                    _ => break,
                }
                let (right, value_pos) = parse_value(functions, block, curr_idx)?;
                vals.push(right);
                pos = merge_file_positions(&pos, &value_pos);
            },
            _ => break,
        }
    }

    let operator_precedence = vec![vec![Operator::Mul], vec![Operator::Plus]];

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
                let left = vals.remove(i);
                let right = vals.remove(i);
                let op = ops.remove(i);
                vals.insert(i, Expression::BinaryOperation(Box::new(left), op, Box::new(right)));
            } else {
                break;
            }
        }
    }

    assert_eq!(vals.len(), 1);
    assert_eq!(ops.len(), 0);

    Ok((vals.pop().unwrap(), pos))
}