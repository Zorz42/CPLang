mod function;

use crate::compiler::parser::block::Block;
use crate::compiler::parser::function::FunctionSignature;

pub fn generate_code(functions: &Vec<(FunctionSignature, Block)>) -> String {
    let mut code = String::new();

    for (signature, block) in functions {
        let function_code = function::generate_function(signature, block);
        code.push_str(&function_code);
    }

    code
}