use crate::compiler::error::CompilerResult;
use crate::compiler::generator::expression::{generate_expression, ValueType};
use crate::compiler::generator::GlobalContext;
use crate::compiler::generator::print::generate_print_statement;
use crate::compiler::generator::variable::generate_variable_declaration;
use crate::compiler::parser::block::Block;
use crate::compiler::parser::Statement;

pub fn generate_block(context: &mut GlobalContext, block: &Block) -> CompilerResult<String> {
    let prev_variables = context.variables.clone();

    let mut code = "{\n".to_owned();

    for statement in &block.children {
        let new_code = match statement {
            Statement::VariableDeclaration(declaration) => {
                generate_variable_declaration(context, declaration)?
            }
            Statement::Block(block) => {
                generate_block(context, block)?
            }
            Statement::Expression(expression) => {
                generate_expression(context, expression)?.0
            }
            Statement::Print(expression) => {
                generate_print_statement(context, expression)?
            }
            Statement::Return(expression) => {
                let (code, typ) = generate_expression(context, expression)?;
                if context.return_type != ValueType::Void && typ != context.return_type {
                    panic!("Return type mismatch");
                }

                context.return_type = typ;
                format!("return {}", code)
            }
        };

        code.push_str(&new_code);
        code.push_str(";\n");

    }

    context.variables = prev_variables;

    code.push_str("}");
    Ok(code)
}