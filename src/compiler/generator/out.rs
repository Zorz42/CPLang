use crate::compiler::error::{CompilerResult};
use crate::compiler::generator::expression::{generate_expression, ValueType};
use crate::compiler::generator::GlobalContext;
use crate::compiler::parser::out::PrintStatement;

fn type_to_printf_format(typ: &ValueType) -> &'static str {
    match typ {
        ValueType::I32 => "d",
        ValueType::F32 => "f",
        ValueType::String => "s",
        ValueType::I64 => "ld",
        ValueType::F64 => "lf",
        ValueType::Boolean => "d",
        ValueType::Reference(typ) => type_to_printf_format(typ),
        ValueType::Struct(_, _) => panic!("Cannot print struct type"),
        ValueType::Void => panic!("Cannot print void type"),
    }
}

pub fn generate_out_statement(context: &mut GlobalContext, expression: &PrintStatement) -> CompilerResult<String> {
    let mut parts = Vec::new();

    for val in &expression.values {
        let (mut code, typ, _) = generate_expression(context, val)?;

        let printf_format = type_to_printf_format(&typ);
        let mut curr_typ = typ;
        while let ValueType::Reference(inner) = curr_typ {
            curr_typ = *inner;
            code.insert(0, '*');
        }

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

    code.push(')');

    Ok(code)
}