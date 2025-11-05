use crate::compiler::error::{merge_file_positions, CompilerError, CompilerResult};
use crate::compiler::parser::expression::parse_expression;
use crate::compiler::parser::Statement;
use crate::compiler::parser::structure::StructDeclaration;
use crate::compiler::parser::typed::{parse_type, Type};
use crate::compiler::tokenizer::{TokenBlock, Token, Keyword, Symbol};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct FunctionSignature {
    pub name: String,
    pub args: Vec<(String, Type)>,
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
            message: "Unexpected token".to_string(),
            position: Some(
                block.children[*curr_idx].1.clone()
            ),
        }),
    }
    *curr_idx += 1;

    loop {
        let arg = match &block.children.get(*curr_idx).map(|x| x.0.clone()) {
            Some(Token::BraceBlock(block)) => {
                res_block = block.clone();
                *curr_idx += 1;
                break;
            }
            Some(Token::Identifier(arg)) => {
                arg.clone()
            }
            _ => return Err(CompilerError {
                message: "Expected block or argument identifier after function signature".to_string(),
                position: Some(
                    block.children[*curr_idx - 1].1.clone()
                ),
            }),
        };
        *curr_idx += 1;

        let type_hint = match &block.children.get(*curr_idx).map(|x| x.0.clone()) {
            Some(Token::Symbol(Symbol::Colon)) => {
                // type hint
                *curr_idx += 1;
                parse_type(block, curr_idx)?
            }
            _ => Type::Any,
        };

        res_signature.args.push((arg, type_hint));
    }

    Ok((res_signature, res_block))
}

pub fn parse_return_statement(structs: &Vec<StructDeclaration>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<Option<Statement>> {
    if block.children[*curr_idx].0 != Token::Keyword(Keyword::Return) {
        return Ok(None);
    }
    let pos1 = block.children[*curr_idx].1.clone();
    *curr_idx += 1;
    if block.children.len() == *curr_idx {
        return Ok(Some(Statement::Return(None, pos1)))
    }

    let (expression, pos2) = parse_expression(structs, block, curr_idx)?;
    Ok(Some(Statement::Return(Some(expression), merge_file_positions(&pos1, &pos2))))
}