mod function;
mod block;
mod expression;
mod variable;
mod print;
mod statement;

use std::collections::{HashMap, HashSet};
use crate::compiler::error::{CompilerError, CompilerResult};
use crate::compiler::generator::expression::{setup_default_operators, ValueType};
use crate::compiler::generator::function::generate_function;
use crate::compiler::parser::block::Block;
use crate::compiler::parser::expression::Operator;
use crate::compiler::parser::function::FunctionSignature;

pub struct GlobalContext {
    pub functions: Vec<(FunctionSignature, Block)>,
    pub operators: HashMap<(ValueType, Operator, ValueType), (Box<dyn Fn(String, String) -> String>, ValueType)>,
    pub variables: HashMap<String, ValueType>,
    pub code: String, // the code generated so far
    pub taken_function_names: HashSet<String>,
    pub return_type: ValueType,
}

impl GlobalContext {
    pub fn get_variable_type(&self, name: &str) -> Option<ValueType> {
        self.variables.get(name).cloned()
    }
}

pub fn generate_code(functions: Vec<(FunctionSignature, Block)>) -> CompilerResult<String> {
    let mut context = GlobalContext {
        functions,
        operators: HashMap::new(),
        variables: HashMap::new(),
        code: "#include<stdio.h>\n\n".to_owned(),
        taken_function_names: HashSet::new(),
        return_type: ValueType::Void,
    };

    setup_default_operators(&mut context);

    // find main function
    let (main_signature, main_block) = context.functions.iter().find(|f| f.0.name == "main").expect("No main function found").clone();
    if main_signature.args.len() != 0 {
        return Err(CompilerError {
            message: "Main function should not have arguments".to_string(),
            position: None,
        });
    }

    let (main_name, main_return_type) = generate_function(&mut context, &main_signature, &main_block, &vec![])?;

    if main_return_type != ValueType::Void {
        return Err(CompilerError {
            message: "Main function should not return anything".to_string(),
            position: None,
        });
    }

    context.code.push_str(&format!("int main(){{{main_name}();return 0;}}\n\n"));

    Ok(context.code)
}