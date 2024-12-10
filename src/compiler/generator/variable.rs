use crate::compiler::generator::expression::generate_expression;
use crate::compiler::generator::GlobalContext;
use crate::compiler::parser::variable::VariableDeclaration;

pub fn generate_variable_declaration(context: &mut GlobalContext, declaration: &VariableDeclaration) -> String {
    let value_type2 = context.get_variable_type(&declaration.name);
    let (value_code, value_type) = generate_expression(context, &declaration.value);
    if let Some(value_type2) = value_type2 {
        if value_type != value_type2 {
            panic!("Type mismatch");
        }
        format!("{} = {}", declaration.name, value_code)
    } else {
        context.variables.insert(declaration.name.clone(), value_type.clone());
        format!("{} {} = {}", value_type.to_c_type(), declaration.name, value_code)
    }
}