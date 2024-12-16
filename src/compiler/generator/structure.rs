use crate::compiler::error::CompilerResult;
use crate::compiler::generator::expression::ValueType;
use crate::compiler::generator::GlobalContext;
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