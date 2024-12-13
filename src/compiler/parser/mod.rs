use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::parser::block::{parse_block, Block};
use crate::compiler::parser::expression::Expression;
use crate::compiler::parser::function::{parse_function_declaration, FunctionSignature};
use crate::compiler::parser::print::PrintStatement;
use crate::compiler::parser::variable::VariableDeclaration;
use crate::compiler::tokenizer::TokenBlock;

pub mod function;
pub mod block;
pub mod expression;
pub mod variable;
pub mod print;
mod return_statement;

#[derive(Debug, Clone)]
pub enum Statement {
    VariableDeclaration(VariableDeclaration),
    Block(Block),
    Expression(Expression),
    Print(PrintStatement),
    Return(Expression, FilePosition),
}

pub fn parse_tokens(program_block: &TokenBlock) -> CompilerResult<Vec<(FunctionSignature, Block)>> {
    let mut curr_idx = 0;
    let mut function_declarations = Vec::new();
    while curr_idx < program_block.children.len() {
        let declaration = parse_function_declaration(&program_block, &mut curr_idx)?;
        function_declarations.push(declaration);
    }

    //println!("{:?}", function_declarations);

    let mut res = Vec::new();

    let functions = function_declarations.iter().map(|(signature, _)| signature.clone()).collect::<Vec<_>>();

    let mut found_main = false;
    for (signature, function_block) in function_declarations {
        //println!("Parsing {}", signature.name);
        let parsed_block = parse_block(&functions, &function_block, &mut 0)?;

        if signature.name == "main" {
            found_main = true;
        }

        //println!("{:?}", parsed_block);
        res.push((signature, parsed_block));
    }

    if !found_main {
        return Err(CompilerError { message: "No main function found".to_string(), position: None });
    }

    Ok(res)
}