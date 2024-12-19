use crate::compiler::error::{CompilerError, CompilerResult};
use crate::compiler::generator::expression::{generate_expression, ValueType};
use crate::compiler::generator::GlobalContext;
use crate::compiler::generator::structure::generate_field_access;
use crate::compiler::parser::variable::VariableDeclaration;

pub fn generate_variable_declaration(context: &mut GlobalContext, declaration: &VariableDeclaration) -> CompilerResult<String> {
    let mut var_name = declaration.name[0].clone();

    let value_type2 = context.get_variable_type(&var_name);
    let (value_code, value_type, _) = generate_expression(context, &declaration.value)?;
    if let Some(mut value_type2) = value_type2 {
        for field in declaration.name.iter().skip(1) {
            while let ValueType::Reference(inner) = value_type2 {
                var_name = format!("*({})", var_name);
                value_type2 = *inner;
            }

            (var_name, value_type2) = generate_field_access(context, var_name.clone(), value_type2.clone(), field, &declaration.pos)?;
        }

        while let ValueType::Reference(inner) = value_type2 {
            var_name = format!("*({})", var_name);
            value_type2 = *inner;
        }

        if value_type != value_type2 {
            let message = format!("Cannot assign {:?} to type {:?}", value_type, value_type2);
            return Err(CompilerError{
                message,
                position: Some(declaration.pos.clone()),
            })
        }
        Ok(format!("{} = {}", var_name, value_code))
    } else {
        if declaration.name.len() != 1 {
            return Err(CompilerError {
                message: format!("Variable {} was not declared yet", var_name),
                position: Some(declaration.pos.clone()),
            })
        }

        context.variables.insert(var_name.clone(), value_type.clone());
        Ok(format!("{} {} = {}", value_type.to_c_type(context)?, var_name, value_code))
    }
}