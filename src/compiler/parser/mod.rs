use crate::compiler::parser::block::{parse_block, Block};
use crate::compiler::parser::expression::Expression;
use crate::compiler::parser::function::{parse_function_declaration, FunctionSignature};
use crate::compiler::parser::variable::VariableDeclaration;
use crate::compiler::tokenizer::TokenBlock;

pub mod function;
pub mod block;
pub mod expression;
pub mod variable;

#[derive(Debug)]
pub enum Statement {
    VariableDeclaration(VariableDeclaration),
    Block(Block),
    Expression(Expression),
}

pub fn parse_tokens(program_block: &TokenBlock) -> Vec<(FunctionSignature, Block)> {
    let mut curr_idx = 0;
    let mut function_declarations = Vec::new();
    while curr_idx < program_block.children.len() {
        let declaration = parse_function_declaration(&program_block, &mut curr_idx);
        function_declarations.push(declaration);
    }

    //println!("{:?}", function_declarations);

    let mut res = Vec::new();

    let mut found_main = false;
    for (signature, function_block) in function_declarations {
        //println!("Parsing {}", signature.name);
        let mut idx = 0;
        let parsed_block = parse_block(&function_block, &mut idx);

        if signature.name == "main" {
            found_main = true;
        }

        //println!("{:?}", parsed_block);
        res.push((signature, parsed_block));
    }

    if !found_main {
        panic!("No main function found");
    }

    res
}