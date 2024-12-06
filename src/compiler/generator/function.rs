use crate::compiler::parser::block::Block;
use crate::compiler::parser::function::FunctionSignature;

pub fn generate_function(signature: &FunctionSignature, block: &Block) -> String {
    let mut code = String::new();

    code.push_str(&format!("void {}() {{\n", signature.name));
    code.push_str("}\n");

    code
}