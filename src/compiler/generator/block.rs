use crate::compiler::error::{CompilerResult};
use crate::compiler::generator::expression::{generate_expression};
use crate::compiler::generator::function::generate_return_statement;
use crate::compiler::generator::GlobalContext;
use crate::compiler::generator::out::generate_out_statement;
use crate::compiler::generator::statement::{generate_if_statement, generate_while_statement};
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
                generate_out_statement(context, expression)?
            }
            Statement::Return(expression, pos) => {
                generate_return_statement(context, expression.as_ref(), pos)?
            }
            Statement::If(statement) => {
                generate_if_statement(context, statement)?
            }
            Statement::While(statement) => {
                generate_while_statement(context, statement)?
            }
        };

        code.push_str(&new_code);
        code.push_str(";\n");

    }

    context.variables = prev_variables;

    code.push('}');
    Ok(code)
}