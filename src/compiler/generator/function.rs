use crate::compiler::generator::block::generate_block;
use crate::compiler::generator::GlobalContext;
use crate::compiler::parser::block::Block;
use crate::compiler::parser::function::FunctionSignature;

pub fn generate_function_signature(signature: &FunctionSignature) -> String {
    let mut code = String::new();

    code.push_str(&format!("void f{}()", signature.name));

    code
}

pub fn generate_function(context: &mut GlobalContext, signature: &FunctionSignature, block: &Block) -> String {
    let mut code = String::new();

    code.push_str(generate_function_signature(signature).as_str());
    code.push_str(&generate_block(context, block));
    code.push_str("\n\n");

    code
}