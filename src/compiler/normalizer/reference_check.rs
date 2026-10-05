use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::normalizer::ValuePhysicality;
use crate::compiler::normalizer::ir::{IR, IRExpression, IRStatement, IRType, IRTypeLabel, IRVariableLabel};
use crate::compiler::normalizer::ir_pass::IRPass;
use crate::compiler::normalizer::symbol_table::SymbolTable;
use cplang::compiler::normalizer::ir::IRInstance;
use std::collections::HashMap;
// this file implements the pass of IR that happens in normalizer and checks
// that all lhs in assignments are physical and resolves all autorefs

struct CheckRefsPass<'a> {
    autorefs: Vec<i32>,
    error: Option<CompilerError>,
    symbol_table: &'a mut SymbolTable,
    new_vars: Vec<IRVariableLabel>,
    types: HashMap<IRTypeLabel, IRType>,
    variable_types: Vec<IRTypeLabel>,
    curr_type_label: IRTypeLabel,
}

impl CheckRefsPass<'_> {
    fn report_error(&mut self, error: CompilerError) {
        if self.error.is_none() {
            self.error = Some(error);
        }
    }

    fn new_type_label(&mut self) -> IRTypeLabel {
        while self.types.contains_key(&self.curr_type_label) {
            self.curr_type_label += 1;
        }
        self.curr_type_label
    }
}

impl IRPass for CheckRefsPass<'_> {
    fn post_map_instance(&mut self, mut instance: IRInstance) -> IRInstance {
        instance.variables.append(&mut self.new_vars);
        instance
    }

    fn post_map_statement(&mut self, statement: IRStatement) -> IRStatement {
        match statement {
            IRStatement::Assignment { assign_to, value, pos } => {
                let is_phys = is_expression_physical(&assign_to);
                if is_phys == ValuePhysicality::Temporary {
                    self.report_error(CompilerError {
                        message: "Left hand side is non-assignable.".to_owned(),
                        position: Some(pos),
                    });
                }
                IRStatement::Assignment { assign_to, value, pos }
            }
            _ => statement,
        }
    }

    fn pre_map_expression(&mut self, expression: IRExpression) -> IRExpression {
        match expression {
            IRExpression::AutoRef {
                expression,
                autoref_label,
                type_label,
            } => {
                let mut curr_type = self.types[&type_label].clone();
                let mut expression = *expression;
                let ref_depth = self.autorefs[autoref_label];
                if ref_depth > 0 {
                    for _ in 0..ref_depth {
                        expression = IRExpression::Reference {
                            expression: Box::new(expression),
                            occupant: None,
                            type_label,
                            pos: FilePosition::unknown(),
                        };
                        curr_type = IRType::Reference(Box::new(curr_type));
                        let type_label = self.new_type_label();
                        self.types.insert(type_label, curr_type.clone());
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
            _ => expression,
        }
    }

    fn post_map_expression(&mut self, expression: IRExpression) -> IRExpression {
        match expression {
            IRExpression::Reference {
                expression,
                pos,
                mut occupant,
                type_label,
            } => {
                let is_phys = is_expression_physical(&expression);
                if is_phys == ValuePhysicality::Temporary {
                    let var_label = self.symbol_table.new_variable_label();
                    self.variable_types.push(type_label);
                    occupant = Some(var_label);
                    self.new_vars.push(var_label);
                }
                IRExpression::Reference {
                    expression,
                    pos,
                    occupant,
                    type_label,
                }
            }
            _ => expression,
        }
    }
}

pub fn check_references(ir: IR, autorefs: Vec<i32>, symbol_table: &mut SymbolTable) -> CompilerResult<IR> {
    let mut passer = CheckRefsPass {
        autorefs,
        error: None,
        symbol_table,
        new_vars: Vec::new(),
        types: ir.types.clone(),
        variable_types: ir.variable_types.clone(),
        curr_type_label: 1,
    };
    let mut ir = passer.pass_ir(ir);
    ir.types = passer.types;
    ir.variable_types = passer.variable_types;
    passer.error.map_or(Ok(ir), Err)
}

fn is_expression_physical(expression: &IRExpression) -> ValuePhysicality {
    match expression {
        IRExpression::Variable { .. } | IRExpression::Dereference { .. } | IRExpression::FieldAccess { .. } => ValuePhysicality::Physical,
        IRExpression::Reference { .. } | IRExpression::StructInitialization { .. } | IRExpression::Constant { .. } | IRExpression::InstanceCall { .. } => {
            ValuePhysicality::Temporary
        }
        IRExpression::BuiltinFunctionCall(call) => call.get_value_physicality(),
        IRExpression::AutoRef { .. } => unreachable!(),
    }
}
