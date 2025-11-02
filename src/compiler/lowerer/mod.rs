use crate::compiler::parser::{Statement, AST};
use crate::compiler::parser::block::Block;

pub fn lower_ast(mut ast: AST) -> AST {
    ast.functions = ast.functions.into_iter().map(|(sign, block)| {
        (sign, lower_block(block))
    }).collect::<Vec<_>>();

    ast
}

fn lower_statement(statement: Statement) -> Statement {
    statement
}

fn lower_block(mut block: Block) -> Block {
    block.children = block.children.into_iter().map(lower_statement).collect::<Vec<_>>();
    block
}