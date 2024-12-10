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
    Identifier(String),
    BinaryOperation(Box<Expression>, Operator, Box<Expression>),
}

// only looks for a single value (if parentheses are used, it will parse whole expression)
fn parse_value(block: &TokenBlock, curr_idx: &mut usize) -> Expression {
    match &block.children[*curr_idx] {
        Token::Constant(Constant::Integer(int)) => {
            *curr_idx += 1;
            Expression::Integer(*int)
        },
        Token::Constant(Constant::Float(float)) => {
            *curr_idx += 1;
            Expression::Float(*float)
        },
        Token::Constant(Constant::String(string)) => {
            *curr_idx += 1;
            Expression::String(string.clone())
        },
        Token::Constant(Constant::Boolean(boolean)) => {
            *curr_idx += 1;
            Expression::Boolean(*boolean)
        },
        Token::Identifier(identifier) => {
            *curr_idx += 1;
            Expression::Identifier(identifier.clone())
        },
        Token::Symbol(Symbol::LeftBracket) => {
            *curr_idx += 1;
            let res = parse_expression(block, curr_idx);
            match &block.children[*curr_idx] {
                Token::Symbol(Symbol::RightBracket) => {
                    *curr_idx += 1;
                    res
                },
                _ => panic!("Expected right bracket"),
            }
        },
        _ => panic!("Expected value"),
    }
}

// looks for operators and values
pub fn parse_expression(block: &TokenBlock, curr_idx: &mut usize) -> Expression {
    let mut vals = Vec::new();
    let mut ops = Vec::new();
    vals.push(parse_value(block, curr_idx));
    while *curr_idx < block.children.len() {
        match &block.children[*curr_idx] {
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
                let right = parse_value(block, curr_idx);
                vals.push(right);
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

    vals.pop().unwrap()
}