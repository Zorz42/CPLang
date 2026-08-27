use crate::CompilerError;
use crate::compiler::error::CompilerResult;
use crate::compiler::normalizer::ir::IRConstant;
use crate::compiler::normalizer::ir::{IR, IRBlock, IRExpression, IRStatement};

// figures out wheter the statement will return from the function in all control paths
fn is_statement_terminal(statement: &IRStatement) -> bool {
    match statement {
        IRStatement::Block { block } =>
            is_block_terminal(block),
        IRStatement::If { condition: _, block, else_block } =>
            is_block_terminal(block) && is_block_terminal(else_block),
        IRStatement::While { condition, block } =>
            is_loop_infinite(condition, block),
        IRStatement::Return { .. } => true,
        IRStatement::Expression { .. } |
        IRStatement::Assignment { .. } |
        IRStatement::Break { .. } |
        IRStatement::Continue { .. } => false,
    }
}

fn is_block_terminal(block: &IRBlock) -> bool {
    block.statements.iter().any(is_statement_terminal)
}

pub fn analyze_return_statements(ir: &IR) -> CompilerResult<()> {
    for instance in &ir.instances {
        if !is_block_terminal(&instance.block) {
            return Err(CompilerError {
                message: "This function does not return a value in all control paths".to_owned(),
                position: Some(instance.pos),
            });
        }
    }
    Ok(())
}

fn is_loop_infinite(condition: &IRExpression, block: &IRBlock) -> bool {
    matches!(condition, IRExpression::Constant { constant: IRConstant::Bool(true) }) &&
        !can_break_out(block, 1)
}

fn can_break_out(block: &IRBlock, loop_depth: i32) -> bool {
    for statement in &block.statements {
        let res = match statement {
            IRStatement::Break { depth, pos: _ } =>
                *depth >= loop_depth,
            IRStatement::Block { block } =>
                can_break_out(block, loop_depth),
            IRStatement::If { condition: _, block, else_block } =>
                can_break_out(block, loop_depth) || can_break_out(else_block, loop_depth),
            IRStatement::While { condition: _, block } =>
                can_break_out(block, loop_depth + 1),
            IRStatement::Continue { depth, pos: _ } =>
                *depth > loop_depth,
            IRStatement::Expression { .. } |
            IRStatement::Return { .. } |
            IRStatement::Assignment { .. } => false,
        };
        if res {
            return true;
        }
    }
    false
}