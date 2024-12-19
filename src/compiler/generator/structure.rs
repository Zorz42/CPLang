use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::generator::expression::{generate_expression, ValueType};
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