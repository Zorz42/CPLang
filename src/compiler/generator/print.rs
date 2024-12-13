use crate::compiler::error::{CompilerError, CompilerResult};
use crate::compiler::generator::expression::{generate_expression, ValueType};
use crate::compiler::generator::GlobalContext;
use crate::compiler::parser::print::PrintStatement;

pub fn generate_print_statement(context: &mut GlobalContext, expression: &PrintStatement) -> CompilerResult<String> {
    let mut parts = Vec::new();

    for val in &expression.values {
        let (code, typ) = generate_expression(context, val)?;

        let printf_format = match typ {
            ValueType::I32 => "d",
            ValueType::F32 => "f",
            ValueType::String => "s",
            ValueType::I64 => "ld",
            ValueType::F64 => "lf",
            ValueType::Boolean => "d",
            ValueType::Void => return Err(CompilerError {
                message: "Cannot print void type".to_string(),
                position: Some(expression.pos.clone()),
            }),
        };

        parts.push((code, printf_format));
    }

    let mut code = "printf(\"".to_owned();

    for (_, format) in &parts {
        code.push_str(&format!("%{}", format));
    }

    code.push_str("\\n\"");

    for (part, _) in &parts {
        code.push_str(&format!(", {}", part));
    }

    code.push_str(")");

    Ok(code)
}