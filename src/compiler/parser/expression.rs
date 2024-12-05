use crate::compiler::tokenizer::{TokenBlock, Constant, Token};

#[derive(Debug)]
pub enum Operator {
    Plus,
    Times,
}

#[derive(Debug)]
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
fn parse_expression(block: &TokenBlock, curr_idx: &mut usize) -> Expression {
    let val1 = parse_value(block, curr_idx);
    if *curr_idx >= block.children.len() {
        return val1;
    }

    match &block.children[*curr_idx] {
        Token::Symbol(symbol) => {
            let operator = match symbol {
                crate::compiler::tokenizer::Symbol::Plus => Operator::Plus,
                crate::compiler::tokenizer::Symbol::Star => Operator::Times,
                _ => return val1,
            };
            *curr_idx += 1;
            let val2 = parse_value(block, curr_idx);
            Expression::BinaryOperation(Box::new(val1), operator, Box::new(val2))
        },
        _ => val1,
    }
}