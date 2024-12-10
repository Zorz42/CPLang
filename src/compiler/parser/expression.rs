use crate::compiler::tokenizer::{TokenBlock, Constant, Token, Symbol};

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Operator {
    Plus,
    Times,
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
        _ => panic!("Expected value"),
    }
}

// looks for operators and values
pub fn parse_expression(block: &TokenBlock, curr_idx: &mut usize) -> Expression {
    let mut res = parse_value(block, curr_idx);
    while *curr_idx < block.children.len() {
        match &block.children[*curr_idx] {
            Token::Symbol(symbol) => {
                match symbol {
                    Symbol::Plus => {
                        *curr_idx += 1;
                        let right = parse_value(block, curr_idx);
                        res = Expression::BinaryOperation(Box::new(res), Operator::Plus, Box::new(right));
                    },
                    Symbol::Star => {
                        *curr_idx += 1;
                        let right = parse_value(block, curr_idx);
                        res = Expression::BinaryOperation(Box::new(res), Operator::Times, Box::new(right));
                    },
                    _ => break,
                }
            },
            _ => break,
        }
    }

    res
}