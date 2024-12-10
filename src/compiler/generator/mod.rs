mod function;
mod block;
mod expression;
mod variable;

use std::collections::HashMap;
use crate::compiler::generator::expression::{setup_default_operators, ValueType};
use crate::compiler::parser::block::Block;
use crate::compiler::parser::expression::Operator;
use crate::compiler::parser::function::FunctionSignature;

pub struct GlobalContext {
    pub functions: Vec<FunctionSignature>,
    pub operators: HashMap<(ValueType, Operator, ValueType), (Box<dyn Fn(String, String) -> String>, ValueType)>,
    pub variables: HashMap<String, ValueType>,
}

impl GlobalContext {
    pub fn get_variable_type(&self, name: &str) -> Option<ValueType> {
        self.variables.get(name).cloned()
    }
}

pub fn generate_code(functions: &Vec<(FunctionSignature, Block)>) -> String {
    let mut code = "#include<stdio.h>\nint main(){fmain();return 0;}\n".to_owned();

    let mut context = GlobalContext {
        functions: Vec::new(),
        operators: HashMap::new(),
        variables: HashMap::new(),
    };

    for (signature, _) in functions {
        context.functions.push(signature.clone());
    }

    setup_default_operators(&mut context);

    for (signature, block) in functions {
        let function_code = function::generate_function(&mut context, signature, block);
        code.push_str(&function_code);
    }

    code
}