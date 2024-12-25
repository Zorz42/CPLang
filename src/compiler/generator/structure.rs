use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::generator::expression::{generate_expression, ValueType};
use crate::compiler::generator::function::generate_function;
use crate::compiler::generator::GlobalContext;
use crate::compiler::parser::expression::Expression;
use crate::compiler::parser::structure::StructDeclaration;

pub fn generate_struct(context: &mut GlobalContext, declaration: &StructDeclaration, field_types: &Vec<ValueType>) -> CompilerResult<String> {
    if let Some(res) = context.generated_structs.get(&(declaration.name.clone(), field_types.clone())) {
        return Ok(res.clone());
    }

    let mut idx = 0;
    while context.taken_symbol_names.contains(&format!("cplang_{}{}", declaration.name, idx)) {
        idx += 1;
    }

    let c_name = format!("cplang_{}{}", declaration.name, idx);
    context.taken_symbol_names.insert(c_name.clone());
    context.generated_structs.insert((declaration.name.clone(), field_types.clone()), c_name.clone());

    let mut code = String::new();
    code.push_str(&format!("typedef struct {} {{\n", c_name));
    for (field_name, field_type) in declaration.fields.iter().zip(field_types.iter()) {
        code.push_str(&format!("    {} {};\n", field_type.to_c_type(context)?, field_name));
    }
    code.push_str(&format!("}} {};\n", c_name));
    context.code.push_str(&code);

    Ok(c_name)
}

pub fn generate_field_access(context: &mut GlobalContext, mut expr_code: String, mut expr_type: ValueType, field: &str, pos: &FilePosition) -> CompilerResult<(String, ValueType)> {
    // deref while you can
    while let ValueType::Reference(inner) = expr_type {
        expr_type = *inner;
        expr_code = format!("*({})", expr_code);
    }

    match expr_type {
        ValueType::Struct(name, fields) => {
            let declaration = context.structs.iter().find(|s| s.name == name).expect("Struct not found").clone();
            let field_index = declaration.fields.iter().position(|f| f == field);
            let field_index = match field_index {
                Some(val) => val,
                None => return Err(CompilerError {
                    message: format!("Field {} not found in struct {}", field, name),
                    position: Some(pos.clone()),
                }),
            };
            let field_type = fields[field_index].clone();

            Ok((format!("({}).{}", expr_code, field), field_type))
        }
        _ => Err(CompilerError {
            message: format!("Field access on non-struct type {:?}", expr_type),
            position: Some(pos.clone()),
        })
    }
}

pub fn generate_struct_instantiation(context: &mut GlobalContext, name: String, args: &Vec<Expression>) -> CompilerResult<(String, ValueType)> {
    let declaration = context.structs.iter().find(|s| s.name == *name).expect("Struct not found").clone();
    let mut arg_types = Vec::new();
    let mut arg_codes = Vec::new();
    for arg in args {
        let res = generate_expression(context, arg)?;
        arg_types.push(res.1);
        arg_codes.push(res.0);
    }

    let c_name = generate_struct(context, &declaration, &arg_types)?;
    let typ = ValueType::Struct(name, arg_types);

    let mut code = String::new();
    code.push_str(&format!("({c_name}){{"));
    for arg in &arg_codes {
        code.push_str(arg);
        code.push_str(",");
    }
    if !arg_codes.is_empty() {
        code.pop();
    }
    code.push_str("}");
    Ok((code, typ))
}

pub fn generate_method_call(context: &mut GlobalContext, name: &str, expr: &Expression, expr_pos: &FilePosition, args: &Vec<Expression>) -> CompilerResult<(String, ValueType)> {
    /*let (signature, block) = context.functions.iter().find(|f| f.0.name == *name).expect("Function not found").clone();
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
    Ok((code, return_val))*/

    let (expr_code, expr_type, is_phys) = generate_expression(context, expr)?;

    if !is_phys {
        return Err(CompilerError {
            message: "Cannot call method on non-physical value".to_string(),
            position: Some(expr_pos.clone()),
        });
    }

    let struct_name = if let ValueType::Struct(name, _) = expr_type.clone() {
        name
    } else {
        return Err(CompilerError {
            message: "Method call on non-struct type".to_string(),
            position: Some(expr_pos.clone()),
        });
    };

    let struct_declaration = context.structs.iter().find(|s| s.name == struct_name).expect("Struct not found").clone();
    let method_declaration = struct_declaration.methods.iter().find(|m| m.0.name == *name);

    let (signature, block) = if let Some(x) = method_declaration {
        x
    } else {
        return Err(CompilerError {
            message: format!("Method {} not found in struct {}", name, struct_name),
            position: Some(expr_pos.clone()),
        });
    };

    let mut signature = signature.clone();
    signature.args.insert(0, "self".to_owned());

    let mut arg_types = Vec::new();
    let mut arg_codes = Vec::new();
    arg_types.push(ValueType::Reference(Box::new(expr_type)));
    arg_codes.push(format!("&({expr_code})"));

    for arg in args {
        let res = generate_expression(context, arg)?;
        arg_types.push(res.1);
        arg_codes.push(res.0);
    }

    let (func_name, return_val) = generate_function(context, &signature, block, &arg_types)?;

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