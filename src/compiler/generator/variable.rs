use crate::compiler::error::{CompilerError, CompilerResult};
use crate::compiler::generator::expression::generate_expression;
use crate::compiler::generator::expression::ValueType::Reference;
use crate::compiler::generator::GlobalContext;
use crate::compiler::parser::variable::VariableDeclaration;

pub fn generate_variable_declaration(context: &mut GlobalContext, declaration: &VariableDeclaration) -> CompilerResult<String> {
    let value_type2 = context.get_variable_type(&declaration.name);
    let (value_code, value_type) = generate_expression(context, &declaration.value)?;
    if let Some(value_type2) = value_type2 {
        // check if it is reference assignment
        if Reference(Box::new(value_type.clone())) == value_type2 {
            return Ok(format!("*{} = {}", declaration.name, value_code))
        }

        if value_type != value_type2 {
            let message = format!("Variable {} was declared with type {:?} but reassigned with type {:?}", declaration.name, value_type2, value_type);
            return Err(CompilerError{
                message,
                position: Some(declaration.pos.clone()),
            })
        }
        Ok(format!("{} = {}", declaration.name, value_code))
    } else {
        context.variables.insert(declaration.name.clone(), value_type.clone());
        Ok(format!("{} {} = {}", value_type.to_c_type(context)?, declaration.name, value_code))
    }
}