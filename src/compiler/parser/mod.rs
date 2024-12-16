use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::parser::block::{parse_block, Block};
use crate::compiler::parser::expression::Expression;
use crate::compiler::parser::function::{parse_function_declaration, FunctionSignature};
use crate::compiler::parser::out::PrintStatement;
use crate::compiler::parser::statement::{IfStatement, WhileStatement};
use crate::compiler::parser::structure::{parse_struct_declaration, StructDeclaration};
use crate::compiler::parser::variable::VariableDeclaration;
use crate::compiler::tokenizer::TokenBlock;

pub mod function;
pub mod block;
pub mod expression;
pub mod variable;
pub mod out;
pub mod statement;
pub mod structure;

#[derive(Debug, Clone)]
pub enum Statement {
    VariableDeclaration(VariableDeclaration),
    Block(Block),
    Expression(Expression),
    Print(PrintStatement),
    Return(Expression, FilePosition),
    IfStatement(IfStatement),
    WhileStatement(WhileStatement),
}

pub fn parse_tokens(program_block: &TokenBlock) -> CompilerResult<(Vec<(FunctionSignature, Block)>, Vec<StructDeclaration>)> {
    let mut curr_idx = 0;
    let mut function_declarations = Vec::new();
    let mut struct_declarations = Vec::new();
    while curr_idx < program_block.children.len() {
        if let Some(struct_declaration) = parse_struct_declaration(program_block, &mut curr_idx)? {
            struct_declarations.push(struct_declaration);
        } else {
            let declaration = parse_function_declaration(program_block, &mut curr_idx)?;
            function_declarations.push(declaration);
        }
    }

    //println!("{:?}", function_declarations);

    let mut res = Vec::new();

    let functions = function_declarations.iter().map(|(signature, _)| signature.clone()).collect::<Vec<_>>();

    let mut found_main = false;
    for (signature, function_block) in function_declarations {
        //println!("Parsing {}", signature.name);
        let parsed_block = parse_block(&functions, &struct_declarations, &function_block)?;

        if signature.name == "main" {
            found_main = true;
        }

        //println!("{:?}", parsed_block);
        res.push((signature, parsed_block));
    }

    if !found_main {
        return Err(CompilerError { message: "No main function found".to_string(), position: None });
    }

    Ok((res, struct_declarations))
}