mod function;
mod block;
mod expression;
mod variable;
mod out;
mod statement;
mod structure;

use std::collections::{HashMap, HashSet};
use crate::compiler::error::{CompilerError, CompilerResult};
use crate::compiler::generator::expression::{setup_default_operators, ValueType};
use crate::compiler::generator::function::generate_function;
use crate::compiler::parser::block::Block;
use crate::compiler::parser::expression::Operator;
use crate::compiler::parser::function::FunctionSignature;
use crate::compiler::parser::structure::StructDeclaration;

pub struct GlobalContext {
    pub functions: Vec<(FunctionSignature, Block)>,
    pub operators: HashMap<(ValueType, Operator, ValueType), (Box<dyn Fn(String, String) -> String>, ValueType)>,
    pub variables: HashMap<String, ValueType>,
    pub code: String, // the code generated so far
    pub taken_symbol_names: HashSet<String>,
    pub return_type: ValueType,
    pub generated_functions: HashMap<(FunctionSignature, Vec<ValueType>), (String, Option<ValueType>)>,
    // none means that function is being generated and the return type is not known yet
    pub curr_function_signature: Option<FunctionSignature>,
    pub curr_function_args: Vec<ValueType>,
    pub structs: Vec<StructDeclaration>,
    pub generated_structs: HashMap<(String, Vec<ValueType>), String>,
}

impl GlobalContext {
    pub fn get_variable_type(&self, name: &str) -> Option<ValueType> {
        self.variables.get(name).cloned()
    }
}

pub fn generate_code(functions: Vec<(FunctionSignature, Block)>, structs: Vec<StructDeclaration>) -> CompilerResult<String> {
    let mut context = GlobalContext {
        functions,
        operators: HashMap::new(),
        variables: HashMap::new(),
        code: "#include<stdio.h>\n\n".to_owned(),
        taken_symbol_names: HashSet::new(),
        return_type: ValueType::Void,
        generated_functions: HashMap::new(),
        curr_function_signature: None,
        curr_function_args: Vec::new(),
        structs,
        generated_structs: HashMap::new(),
    };

    setup_default_operators(&mut context);

    // find main function
    let (main_signature, main_block) = context.functions.iter().find(|f| f.0.name == "main").expect("No main function found").clone();
    if !main_signature.args.is_empty() {
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