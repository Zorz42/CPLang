use crate::compiler::parser::block::{parse_block, Block};
use crate::compiler::parser::expression::Expression;
use crate::compiler::parser::function::parse_function_declaration;
use crate::compiler::tokenizer::TokenBlock;

mod function;
mod block;
mod expression;

#[derive(Debug)]
pub enum Statement {
    VariableDeclaration,
    Block(Block),
    Expression(Expression),
}

pub fn parse_tokens(program_block: &TokenBlock) {
    let mut curr_idx = 0;
    let mut function_declarations = Vec::new();
    while curr_idx < program_block.children.len() {
        let declaration = parse_function_declaration(&program_block, &mut curr_idx);
        function_declarations.push(declaration);
    }

    println!("{:?}", function_declarations);

    for declaration in function_declarations {
        println!("Parsing {}", declaration.name);
        let mut idx = 0;
        let parsed_block = parse_block(&declaration.block, &mut idx);

        println!("{:?}", parsed_block);
    }
}