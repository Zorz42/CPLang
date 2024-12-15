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
    context.return_type = ValueType::Void;
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
        if !context.taken_function_names.contains(&function_name) {
            break;
        }
        counter += 1;
    }
    context.taken_function_names.insert(function_name.clone());

    context.generated_functions.insert((signature.clone(), arg_types.clone()), (function_name.clone(), None));

    let mut code = String::new();

    let block_code = generate_block(context, block)?;

    let return_type = context.return_type.clone();
    let return_type_str = return_type.to_c_type();
    let mut args_str = String::new();
    for (typ, name) in arg_types.iter().zip(signature.args.iter()) {
        args_str.push_str(&format!("{} {},", typ.to_c_type(), name));
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

pub fn generate_return_statement(context: &mut GlobalContext, expression: &Expression, pos: &FilePosition) -> CompilerResult<String> {
    let (code, typ) = generate_expression(context, expression)?;
    if context.return_type != ValueType::Void && typ != context.return_type {
        return Err(CompilerError {
            message: format!("Return type mismatch, expected {:?} but got {:?}", context.return_type, typ),
            position: Some(pos.clone()),
        });
    }

    context.return_type = typ.clone();
    let signature = context.curr_function_signature.as_ref().expect("Return statement outside of function");
    let arg_types = context.curr_function_args.clone();
    context.generated_functions.get_mut(&(signature.clone(), arg_types)).map(|val| val.1 = Some(typ.clone()));
    Ok(format!("return {}", code))
}