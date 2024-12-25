use crate::compiler::error::{CompilerError, CompilerResult};
use crate::compiler::generator::expression::{generate_expression, ValueType};
use crate::compiler::generator::GlobalContext;
use crate::compiler::parser::expression::Expression;
use crate::compiler::parser::variable::VariableDeclaration;

pub fn generate_variable_declaration(context: &mut GlobalContext, declaration: &VariableDeclaration) -> CompilerResult<String> {
    let (value_code, typ, _) = generate_expression(context, &declaration.value)?;

    let mut is_new = false;
    if let Expression::Variable(ident, _) = &declaration.assign_to {
        if context.get_variable_type(ident).is_none() {
            context.variables.insert(ident.clone(), typ.clone());
            is_new = true;
        }
    }

    let (mut code, mut typ2, mut is_phys) = generate_expression(context, &declaration.assign_to)?;


    while let ValueType::Reference(inner) = &typ2 {
        if typ == typ2 {
            break;
        }
        code = format!("*{}", code);
        typ2 = *inner.clone();
        is_phys = true;
    }

    if typ != typ2 {
        return Err(CompilerError {
            message: format!("Cannot assign {:?} to type {:?}", typ, typ2),
            position: Some(declaration.pos.clone()),
        });
    }

    if !is_phys {
        return Err(CompilerError {
            message: "Cannot assign to non-physical value".to_string(),
            position: Some(declaration.pos.clone()),
        });
    }

    if is_new {
        Ok(format!("{} {} = {}", typ.to_c_type(context)?, code, value_code))
    } else {
        Ok(format!("{} = {}", code, value_code))
    }
}