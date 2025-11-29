use crate::compiler::error::CompilerResult;
use crate::compiler::parser::ast::Ast;
use crate::compiler::parser::block::parse_block;
use crate::compiler::parser::function::parse_function_declaration;
use crate::compiler::parser::structure::parse_struct_declaration;
use crate::compiler::tokenizer::TokenBlock;

pub mod assignment;
pub mod ast;
pub mod block;
pub mod expression;
pub mod function;
pub mod out;
pub mod statement;
pub mod structure;
pub mod typed;
/*
Parser converts tokens into AST (Abstract Syntax Tree). Here is where all the syntax structure parsing happens.
Parser still does not know enough about the program to resolve variable/struct/function names or types or anything else.
It just converts tokens into a tree that represents the layout of the program, which is suitable for further processing.
It does however decide when statements end and operator precedence.
 */

pub fn parse_tokens(mut program_block: TokenBlock) -> CompilerResult<Ast> {
    let mut function_declarations = Vec::new();
    let mut res = Ast {
        functions: Vec::new(),
        structs: Vec::new(),
    };
    while program_block.has_tokens() {
        if let Some(struct_declaration) = parse_struct_declaration(&mut program_block)? {
            res.structs.push(struct_declaration);
        } else {
            let declaration = parse_function_declaration(&mut program_block)?;
            function_declarations.push(declaration);
        }
    }
    for (signature, function_block) in function_declarations {
        let parsed_block = parse_block(&res.structs, function_block)?;

        res.functions.push((signature, parsed_block));
    }

    Ok(res)
}
