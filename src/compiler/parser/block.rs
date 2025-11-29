use crate::compiler::error::CompilerResult;
use crate::compiler::parser::assignment::parse_assignment;
use crate::compiler::parser::ast::{ASTBlock, ASTStatement, ASTStructDeclaration};
use crate::compiler::parser::expression::parse_expression;
use crate::compiler::parser::function::parse_return_statement;
use crate::compiler::parser::out::parse_out_statement;
use crate::compiler::parser::statement::{parse_if_statement, parse_while_statement};
use crate::compiler::tokenizer::{Token, TokenBlock};

pub fn parse_block(structs: &Vec<ASTStructDeclaration>, mut block: TokenBlock) -> CompilerResult<ASTBlock> {
    let mut res = ASTBlock { children: Vec::new() };

    while block.has_tokens() {
        let statement = match block.peek() {
            (Token::BraceBlock(_), _pos) => {
                let sub_block = if let Token::BraceBlock(sub_block) = block.get().0 {
                    sub_block
                } else {
                    unreachable!()
                };

                let statement = ASTStatement::Block {
                    block: parse_block(structs, sub_block)?,
                };
                statement
            }
            _ => {
                if let Some(statement) = parse_return_statement(structs, &mut block)? {
                    statement
                } else if let Some(statement) = parse_out_statement(structs, &mut block)? {
                    statement
                } else if let Some(statement) = parse_if_statement(structs, &mut block)? {
                    statement
                } else if let Some(statement) = parse_while_statement(structs, &mut block)? {
                    statement
                } else {
                    let expression = parse_expression(structs, &mut block)?;
                    if let Some(statement) = parse_assignment(structs, &expression, &mut block)? {
                        statement
                    } else {
                        ASTStatement::Expression { expression }
                    }
                }
            }
        };
        res.children.push(statement);
    }

    Ok(res)
}
