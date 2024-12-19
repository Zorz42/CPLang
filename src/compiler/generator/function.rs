use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::generator::block::generate_block;
use crate::compiler::generator::expression::{generate_expression, ValueType};
use crate::compiler::generator::GlobalContext;
use crate::compiler::parser::block::Block;
use crate::compiler::parser::expression::Expression;
use crate::compiler::parser::function::FunctionSignature;

// it returns generated function name and its return type
pub fn generate_function(context: &mut GlobalContext, signature: &FunctionSignature, block: &Block, arg_types: &Vec<ValueType>) -> CompilerResult<(String, ValueType)> {
    assert_eq!(arg_types.len(), signature.args.len(), "Function signature and arguments mismatch");

    if let Some((name, typ)) = context.generated_functions.get(&(signature.clone(), arg_types.clone())) {
        if let Some(typ) = typ {
            return Ok((name.clone(), typ.clone()));
        } else {
            return Err(CompilerError {
                message: format!("Function {} with non-explicit return type which is not known yet should not be called recursively", signature.name),
                position: None,
            });
        }
    }

    let prev_return_type = context.return_type.clone();
    let prev_function_signature = context.curr_function_signature.clone();
    let prev_function_args = context.curr_function_args.clone();
    let prev_variables = context.variables.clone();
    context.return_type = None;
    context.curr_function_signature = Some(signature.clone());
    context.curr_function_args = arg_types.clone();

    // introduce arguments as variables
    for (typ, name) in arg_types.iter().zip(signature.args.iter()) {
        context.variables.insert(name.clone(), typ.clone());
    }

    // create new function name for c
    let function_prefix = format!("cplang_{}", signature.name);
    let mut counter = 0;
    let mut function_name;
    loop {
        function_name = format!("{}{}", function_prefix, counter);
        if !context.taken_symbol_names.contains(&function_name) {
            break;
        }
        counter += 1;
    }
    context.taken_symbol_names.insert(function_name.clone());

    context.generated_functions.insert((signature.clone(), arg_types.clone()), (function_name.clone(), None));

    let mut code = String::new();

    let block_code = generate_block(context, block)?;

    let return_type = context.return_type.clone().unwrap_or(ValueType::Void);
    let return_type_str = return_type.to_c_type(context)?;
    let mut args_str = String::new();
    for (typ, name) in arg_types.iter().zip(signature.args.iter()) {
        args_str.push_str(&format!("{} {},", typ.to_c_type(context)?, name));
    }
    if !args_str.is_empty() {
        assert_eq!(args_str.pop(), Some(','));
    }

    code.push_str(&format!("{return_type_str} {function_name}({args_str}) {block_code}\n\n"));

    context.code.push_str(&code);

    context.return_type = prev_return_type;
    context.variables = prev_variables;
    context.curr_function_signature = prev_function_signature;
    context.curr_function_args = prev_function_args;

    context.generated_functions.insert((signature.clone(), arg_types.clone()), (function_name.clone(), Some(return_type.clone())));
    Ok((function_name, return_type))
}

pub fn generate_return_statement(context: &mut GlobalContext, expression: Option<&Expression>, pos: &FilePosition) -> CompilerResult<String> {
    if let Some(expression) = expression {
        let (code, typ, _) = generate_expression(context, expression)?;
        if context.return_type.is_some() && Some(typ.clone()) != context.return_type {
            return Err(CompilerError {
                message: format!("Return type mismatch, expected {:?} but got {:?}", context.return_type, typ),
                position: Some(pos.clone()),
            });
        }

        context.return_type = Some(typ.clone());
        let signature = context.curr_function_signature.as_ref().expect("Return statement outside of function");
        let arg_types = context.curr_function_args.clone();
        context.generated_functions.get_mut(&(signature.clone(), arg_types)).map(|val| val.1 = Some(typ.clone()));
        Ok(format!("return {}", code))
    } else {
        Ok("return".to_owned())
    }
}

pub fn generate_function_call(context: &mut GlobalContext, name: &str, args: &Vec<Expression>) -> CompilerResult<(String, ValueType)> {
    let (signature, block) = context.functions.iter().find(|f| f.0.name == *name).expect("Function not found").clone();
    let mut arg_types = Vec::new();
    let mut arg_codes = Vec::new();
    for arg in args {
        let res = generate_expression(context, arg)?;
        arg_types.push(res.1);
        arg_codes.push(res.0);
    }

    let (func_name, return_val) = generate_function(context, &signature, &block, &arg_types)?;

    let mut code = func_name;
    code.push_str("(");
    for arg in &arg_codes {
        code.push_str(arg);
        code.push_str(",");
    }
    if !arg_codes.is_empty() {
        assert_eq!(code.pop(), Some(','));
    }
    code.push_str(")");
    Ok((code, return_val))
}