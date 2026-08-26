use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::normalizer::ValuePhysicality;
use crate::compiler::normalizer::ir::{IR, IRExpression, IRStatement};
use crate::compiler::normalizer::ir_pass::IRPass;

// this file implements the pass of IR that happens in normalizer and checks
// that all lhs in assignments are physical and resolves all autorefs

struct CheckRefsPass {
    autorefs: Vec<i32>,
    error: Option<CompilerError>,
}

impl CheckRefsPass {
    fn report_error(&mut self, error: CompilerError) {
        if self.error.is_none() {
            self.error = Some(error);
        }
    }
}

impl IRPass for CheckRefsPass {
    fn post_map_statement(&mut self, statement: IRStatement) -> IRStatement {
        match statement {
            IRStatement::Assignment { assign_to, value, pos } => {
                let is_phys = is_expression_physical(&assign_to);
                if is_phys == ValuePhysicality::Temporary {
                    self.report_error(CompilerError {
                        message: "Left hand side is non-assignable.".to_string(),
                        position: Some(pos),
                    });
                }
                IRStatement::Assignment { assign_to, value, pos }
            }
            _ => statement
        }
    }

    fn pre_map_expression(&mut self, expression: IRExpression) -> IRExpression {
        match expression {
            IRExpression::AutoRef { expression, autoref_label } => {
                let mut expression = *expression;
                let ref_depth = self.autorefs[autoref_label];
                if ref_depth > 0 {
                    for _ in 0..ref_depth {
                        expression = IRExpression::Reference {
                            expression: Box::new(expression),
                            pos: FilePosition::unknown(),
                        }
                    }
                } else {
                    for _ in 0..-ref_depth {
                        expression = IRExpression::Dereference {
                            expression: Box::new(expression),
                        }
                    }
                }
                expression
            }
            _ => expression
        }
    }

    fn post_map_expression(&mut self, expression: IRExpression) -> IRExpression {
        match expression {
            IRExpression::Reference { expression, pos } => {
                let is_phys = is_expression_physical(&expression);
                if is_phys == ValuePhysicality::Temporary {
                    self.report_error(CompilerError {
                        message: "Cannot reference non-physical value.".to_string(),
                        position: Some(pos),
                    });
                }
                IRExpression::Reference { expression, pos }
            }
            _ => expression
        }
    }
}

pub fn check_refs(ir: IR, autorefs: Vec<i32>) -> CompilerResult<IR> {
    let mut passer = CheckRefsPass {
        autorefs,
        error: None,
    };
    let ir = passer.pass_ir(ir);
    passer.error.map_or(Ok(ir), Err)
}

fn is_expression_physical(expression: &IRExpression) -> ValuePhysicality {
    match expression {
        IRExpression::Variable { .. } |
        IRExpression::Dereference { .. } |
        IRExpression::FieldAccess { .. } => ValuePhysicality::Physical,
        IRExpression::Reference { .. } |
        IRExpression::StructInitialization { .. } |
        IRExpression::Constant { .. } |
        IRExpression::InstanceCall { .. } => ValuePhysicality::Temporary,
        IRExpression::BuiltinFunctionCall(call) => call.get_value_physicality(),
        IRExpression::AutoRef { .. } => unreachable!(),
    }
}
