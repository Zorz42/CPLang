use crate::compiler::error::{merge_file_positions, CompilerError, CompilerResult};
use crate::compiler::parser::expression::parse_expression;
use crate::compiler::parser::Statement;
use crate::compiler::tokenizer::{TokenBlock, Token, Keyword};

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub args: Vec<String>,
}

pub fn parse_function_declaration(block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<(FunctionSignature, TokenBlock)> {
    let mut res_signature = FunctionSignature {
        name: String::new(),
        args: Vec::new(),
    };
    let res_block;

    match &block.children.get(*curr_idx).map(|x| x.0.clone()) {
        Some(Token::Identifier(name)) => {
            res_signature.name = name.clone();
        },
        _ => return Err(CompilerError {
            message: "Expected function name after".to_string(),
            position: Some(
                block.children[*curr_idx - 1].1.clone()
            ),
        }),
    }
    *curr_idx += 1;

    loop {
        match &block.children.get(*curr_idx).map(|x| x.0.clone()) {
            Some(Token::Block(block)) => {
                res_block = block.clone();
                *curr_idx += 1;
                break;
            },
            Some(Token::Identifier(arg)) => {
                res_signature.args.push(arg.clone());
            },
            _ => return Err(CompilerError {
                message: "Expected block or argument identifier after function name".to_string(),
                position: Some(
                    block.children[*curr_idx - 1].1.clone()
                ),
            }),
        }
        *curr_idx += 1;
    }

    Ok((res_signature, res_block))
}

pub fn parse_return_statement(functions: &Vec<FunctionSignature>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<Option<Statement>> {
    if block.children[*curr_idx].0 != Token::Keyword(Keyword::Return) {
        return Ok(None);
    }
    let pos1 = block.children[*curr_idx].1.clone();
    *curr_idx += 1;
    let (expression, pos2) = parse_expression(functions, block, curr_idx)?;
    Ok(Some(Statement::Return(expression, merge_file_positions(&pos1, &pos2))))
}