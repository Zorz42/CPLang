use crate::compiler::parser::expression::{parse_expression, Expression};
use crate::compiler::parser::function::FunctionSignature;
use crate::compiler::tokenizer::{tokenize_string, Constant, Keyword, Token, TokenBlock};

#[derive(Debug, Clone)]
pub struct PrintStatement {
    pub values: Vec<Expression>,
}

fn parse_format_string(functions: &Vec<FunctionSignature>, string: &str) -> Vec<Expression> {
    let mut res = Vec::new();
    let mut curr = String::new();
    let mut in_format = false;
    for c in string.chars() {
        if in_format {
            if c == '}' {
                in_format = false;
                let token_block = TokenBlock { children: tokenize_string(&curr) };
                let mut idx = 0;
                let expression = parse_expression(functions, &token_block, &mut idx);
                assert_eq!(idx, token_block.children.len(), "Did not parse entire format string");

                res.push(expression);

                curr = String::new();
            } else {
                curr.push(c);
            }
        } else {
            if c == '{' {
                in_format = true;
                if !curr.is_empty() {
                    res.push(Expression::String(curr));
                }
                curr = String::new();
            } else {
                curr.push(c);
            }
        }
    }
    if in_format {
        panic!("Expected }} to close format string");
    }

    if !curr.is_empty() {
        res.push(Expression::String(curr));
    }
    res
}

pub fn parse_print_statement(functions: &Vec<FunctionSignature>, block: &TokenBlock, curr_idx: &mut usize) -> Option<PrintStatement> {
    if block.children[*curr_idx] == Token::Keyword(Keyword::Print) {
        *curr_idx += 1;
        match &block.children[*curr_idx] {
            Token::Constant(Constant::String(string)) => {
                *curr_idx += 1;
                Some(PrintStatement {
                    values: parse_format_string(functions, string),
                })
            },
            _ => panic!("Expected string after print keyword"),
        }
    } else {
        None
    }
}