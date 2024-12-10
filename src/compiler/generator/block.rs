use crate::compiler::generator::expression::generate_expression;
use crate::compiler::generator::GlobalContext;
use crate::compiler::generator::print::generate_print_statement;
use crate::compiler::generator::variable::generate_variable_declaration;
use crate::compiler::parser::block::Block;
use crate::compiler::parser::Statement;

pub fn generate_block(context: &mut GlobalContext, block: &Block) -> String {
    let mut code = "{\n".to_owned();

    for statement in &block.children {
        let new_code = match statement {
            Statement::VariableDeclaration(declaration) => {
                generate_variable_declaration(context, declaration)
            }
            Statement::Block(block) => {
                generate_block(context, block)
            }
            Statement::Expression(expression) => {
                generate_expression(context, expression).0
            }
            Statement::Print(expression) => {
                generate_print_statement(context, expression)
            }
        };

        code.push_str(&new_code);
        code.push_str(";\n");

    }

    code.push_str("}");
    code
}