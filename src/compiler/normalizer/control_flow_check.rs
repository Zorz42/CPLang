use crate::CompilerError;
use crate::compiler::error::CompilerResult;
use crate::compiler::normalizer::ir::{IR, IRStatement};
use crate::compiler::normalizer::ir_pass::IRPass;
// this module checks if break and continue statements aren't too deep,
// meaning breaking out of 5 loops while being only 4 loops deep

struct ControlFlowPass {
    loop_depth: i32,
    error: Option<CompilerError>,
}

impl ControlFlowPass {
    fn report_error(&mut self, error: CompilerError) {
        if self.error.is_none() {
            self.error = Some(error);
        }
    }
}


impl IRPass for ControlFlowPass {
    fn pre_map_statement(&mut self, statement: IRStatement) -> IRStatement {
        match statement {
            IRStatement::While { .. } => {
                self.loop_depth += 1;
                statement
            }
            IRStatement::Break { depth, pos } => {
                if depth > self.loop_depth {
                    self.report_error(CompilerError {
                        message: format!("Maximum possible depth for this break statement is {}.", self.loop_depth),
                        position: Some(pos),
                    });
                }
                statement
            }
            IRStatement::Continue { depth, pos } => {
                if depth > self.loop_depth {
                    self.report_error(CompilerError {
                        message: format!("Maximum possible depth for this continue statement is {}.", self.loop_depth),
                        position: Some(pos),
                    });
                }
                statement
            }
            _ => statement
        }
    }

    fn post_map_statement(&mut self, statement: IRStatement) -> IRStatement {
        match statement {
            IRStatement::While { .. } => {
                self.loop_depth -= 1;
                statement
            }
            _ => statement
        }
    }
}

pub fn check_control_flow(ir: IR) -> CompilerResult<IR> {
    let mut pass = ControlFlowPass {
        loop_depth: 0,
        error: None,
    };
    let ir = pass.pass_ir(ir);
    pass.error.map_or(Ok(ir), Err)
}