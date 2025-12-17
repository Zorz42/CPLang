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
        let statement = if let (Token::BraceBlock(_), _pos) = block.peek() {
            let Token::BraceBlock(sub_block) = block.get().0 else { unreachable!() };

            ASTStatement::Block {
                block: parse_block(structs, sub_block)?,
            }
        } else {
            let parsers = [parse_return_statement, parse_out_statement, parse_if_statement, parse_while_statement];

            let mut parser_res = None;

            for parser in parsers {
                if let Some(res) = parser(structs, &mut block)? {
                    parser_res = Some(res);
                    break;
                }
            }

            if let Some(parser_res) = parser_res {
                parser_res
            } else {
                let expression = parse_expression(structs, &mut block)?;
                parse_assignment(structs, expression.clone(), &mut block)?
                    .map_or(ASTStatement::Expression { expression }, |statement| statement)
            }
        };
        res.children.push(statement);
    }

    Ok(res)
}
