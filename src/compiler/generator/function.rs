use crate::compiler::error::CompilerResult;
use crate::compiler::generator::block::generate_block;
use crate::compiler::generator::expression::ValueType;
use crate::compiler::generator::GlobalContext;
use crate::compiler::parser::block::Block;
use crate::compiler::parser::function::FunctionSignature;

// it returns generated function name and its return type
pub fn generate_function(context: &mut GlobalContext, signature: &FunctionSignature, block: &Block, arg_types: &Vec<ValueType>) -> CompilerResult<(String, ValueType)> {
    assert_eq!(arg_types.len(), signature.args.len(), "Function signature and arguments mismatch");

    let prev_return_type = context.return_type.clone();
    let prev_variables = context.variables.clone();
    context.return_type = ValueType::Void;

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

    Ok((function_name, return_type))
}