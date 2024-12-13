use crate::compiler::error::CompilerResult;
use crate::compiler::generator::block::generate_block;
use crate::compiler::generator::expression::generate_expression;
use crate::compiler::generator::GlobalContext;
use crate::compiler::parser::statement::IfStatement;

pub fn generate_if_statement(context: &mut GlobalContext, statement: &IfStatement) -> CompilerResult<String> {
    let (condition_code, _) = generate_expression(context, &statement.condition)?;

    let mut code = format!("if ({}) ", condition_code);
    let block_code = generate_block(context, &statement.block)?;
    code.push_str(&block_code);

    if let Some(else_block) = &statement.else_block {
        code.push_str(" else ");
        let block_code = generate_block(context, else_block)?;
        code.push_str(&block_code);
    }

    Ok(code)
}