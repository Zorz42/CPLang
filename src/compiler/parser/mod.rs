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
    Return(Option<Expression>, FilePosition),
    If(IfStatement),
    While(WhileStatement),
}

pub struct AST {
    pub functions: Vec<(FunctionSignature, Block)>,
    pub structs: Vec<StructDeclaration>,
}

pub fn parse_tokens(program_block: &TokenBlock) -> CompilerResult<AST> {
    let mut curr_idx = 0;
    let mut function_declarations = Vec::new();
    let mut res = AST {
        functions: Vec::new(),
        structs: Vec::new(),
    };
    while curr_idx < program_block.children.len() {
        if let Some(struct_declaration) = parse_struct_declaration(program_block, &mut curr_idx)? {
            res.structs.push(struct_declaration);
        } else {
            let declaration = parse_function_declaration(program_block, &mut curr_idx)?;
            function_declarations.push(declaration);
        }
    }

    let mut found_main = false;
    for (signature, function_block) in function_declarations {
        let parsed_block = parse_block(&res.structs, &function_block)?;

        if signature.name == "main" {
            found_main = true;
        }

        res.functions.push((signature, parsed_block));
    }

    if !found_main {
        return Err(CompilerError { message: "No main function found".to_string(), position: None });
    }

    Ok(res)
}