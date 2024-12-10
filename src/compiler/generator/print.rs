use crate::compiler::generator::GlobalContext;
use crate::compiler::parser::expression::Expression;
use crate::compiler::parser::print::PrintStatement;

pub fn generate_print_statement(context: &mut GlobalContext, expression: &PrintStatement) -> String {
    let mut parts = Vec::new();

    for val in &expression.values {
        parts.push(match val {
            Expression::Integer(val) => {
                (format!("{}", val), "d")
            }
            Expression::Float(val) => {
                (format!("{}", val), "f")
            }
            Expression::String(val) => {
                (format!("\"{}\"", val), "s")
            }
            Expression::Boolean(val) => {
                ((if *val { "true" } else { "false" }).to_owned(), "s")
            }
            Expression::Identifier(val) => {
                unimplemented!()
            }
            Expression::BinaryOperation(_, _, _) => {
                unimplemented!()
            }
        });
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

    code
}