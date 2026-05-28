use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::normalizer::ValuePhysicality;
use crate::compiler::normalizer::ir::{BuiltinFunctionCall, IR, IRBlock, IRExpression, IRInstance, IRStatement};
// this file implements the pass of IR that happens in normalizer and checks
// that all lhs in assignments are physical and resolves all autorefs

/*




*/

pub fn check_refs(mut ir: IR, autorefs: &[i32]) -> CompilerResult<IR> {
    let out: CompilerResult<Vec<_>> = ir.instances.into_iter().map(|instance| check_refs_instance(instance, autorefs)).collect();
    ir.instances = out?;
    Ok(ir)
}

fn check_refs_instance(mut instance: IRInstance, autorefs: &[i32]) -> CompilerResult<IRInstance> {
    instance.block = check_refs_block(instance.block, autorefs)?;
    Ok(instance)
}

fn check_refs_block(mut block: IRBlock, autorefs: &[i32]) -> CompilerResult<IRBlock> {
    let out: CompilerResult<Vec<_>> = block
        .statements
        .into_iter()
        .map(|statement| check_refs_statement(statement, autorefs))
        .collect();
    block.statements = out?;
    Ok(block)
}

fn check_refs_statement(statement: IRStatement, autorefs: &[i32]) -> CompilerResult<IRStatement> {
    let res = match statement {
        IRStatement::Block { block } => IRStatement::Block {
            block: check_refs_block(block, autorefs)?,
        },
        IRStatement::If { condition, block, else_block } => IRStatement::If {
            condition: check_refs_expression(condition, autorefs)?.0,
            block: check_refs_block(block, autorefs)?,
            else_block: else_block.map(|else_block| check_refs_block(else_block, autorefs)).transpose()?,
        },
        IRStatement::While { condition, block } => IRStatement::While {
            condition: check_refs_expression(condition, autorefs)?.0,
            block: check_refs_block(block, autorefs)?,
        },
        IRStatement::Expression { expr } => IRStatement::Expression {
            expr: check_refs_expression(expr, autorefs)?.0,
        },
        IRStatement::Print { expr, type_label } => IRStatement::Print {
            expr: check_refs_expression(expr, autorefs)?.0,
            type_label,
        },
        IRStatement::Return { return_value } => IRStatement::Return {
            return_value: return_value
                .map(|return_value| check_refs_expression(return_value, autorefs))
                .transpose()?
                .map(|x| x.0),
        },
        IRStatement::Assignment { assign_to, value, pos } => {
            let (assign_to, is_phys) = check_refs_expression(assign_to, autorefs)?;

            if is_phys == ValuePhysicality::Temporary {
                return Err(CompilerError {
                    message: "Left hand side is non-assignable".to_string(),
                    position: Some(pos),
                });
            }

            IRStatement::Assignment {
                assign_to,
                value: check_refs_expression(value, autorefs)?.0,
                pos,
            }
        }
    };
    Ok(res)
}

fn check_refs_expression(expression: IRExpression, autorefs: &[i32]) -> CompilerResult<(IRExpression, ValuePhysicality)> {
    let res = match expression {
        IRExpression::Constant { .. } => (expression, ValuePhysicality::Temporary),
        IRExpression::InstanceCall {
            instance_label,
            instance_arguments,
        } => (
            IRExpression::InstanceCall {
                instance_label,
                instance_arguments: instance_arguments
                    .into_iter()
                    .map(|arg| check_refs_expression(arg, autorefs))
                    .collect::<CompilerResult<Vec<_>>>()?
                    .into_iter()
                    .map(|x| x.0)
                    .collect::<Vec<_>>(),
            },
            ValuePhysicality::Temporary,
        ),
        IRExpression::BuiltinFunctionCall(call) => {
            let call = check_refs_builtin(call, autorefs)?;
            let physicality = call.get_value_physicality();
            (IRExpression::BuiltinFunctionCall(call), physicality)
        }
        IRExpression::FieldAccess { field_label, expression } => (
            IRExpression::FieldAccess {
                field_label,
                expression: Box::new(check_refs_expression(*expression, autorefs)?.0),
            },
            ValuePhysicality::Physical,
        ),
        IRExpression::Dereference { expression } => (
            IRExpression::Dereference {
                expression: Box::new(check_refs_expression(*expression, autorefs)?.0),
            },
            ValuePhysicality::Physical,
        ),
        IRExpression::StructInitialization {
            struct_label,
            field_values,
            fields_type_labels,
        } => (
            IRExpression::StructInitialization {
                struct_label,
                fields_type_labels,
                field_values: field_values
                    .into_iter()
                    .map(|val| check_refs_expression(val, autorefs))
                    .collect::<CompilerResult<Vec<_>>>()?
                    .into_iter()
                    .map(|x| x.0)
                    .collect::<Vec<_>>(),
            },
            ValuePhysicality::Temporary,
        ),
        IRExpression::Reference { expression, pos } => {
            let (expression, is_phys) = check_refs_expression(*expression, autorefs)?;
            if is_phys == ValuePhysicality::Temporary {
                return Err(CompilerError {
                    message: "Cannot reference non-physical value.".to_string(),
                    position: Some(pos),
                });
            }
            (
                IRExpression::Reference {
                    expression: Box::new(expression),
                    pos,
                },
                ValuePhysicality::Temporary,
            )
        }
        IRExpression::Variable { .. } => (expression, ValuePhysicality::Physical),
        IRExpression::AutoRef { expression, autoref_label } => {
            let mut expression = *expression;
            let ref_depth = autorefs[autoref_label];
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
            check_refs_expression(expression, autorefs)?
        }
    };
    Ok(res)
}

fn check_refs_builtin(call: BuiltinFunctionCall, autorefs: &[i32]) -> CompilerResult<BuiltinFunctionCall> {
    let res = match call {
        BuiltinFunctionCall::Alloc { typ, num } => BuiltinFunctionCall::Alloc {
            num: Box::new(check_refs_expression(*num, autorefs)?.0),
            typ,
        },
        BuiltinFunctionCall::Index { arr, idx } => BuiltinFunctionCall::Index {
            arr: Box::new(check_refs_expression(*arr, autorefs)?.0),
            idx: Box::new(check_refs_expression(*idx, autorefs)?.0),
        },
        BuiltinFunctionCall::Add { arg1, arg2 } => BuiltinFunctionCall::Add {
            arg1: Box::new(check_refs_expression(*arg1, autorefs)?.0),
            arg2: Box::new(check_refs_expression(*arg2, autorefs)?.0),
        },
        BuiltinFunctionCall::Sub { arg1, arg2 } => BuiltinFunctionCall::Sub {
            arg1: Box::new(check_refs_expression(*arg1, autorefs)?.0),
            arg2: Box::new(check_refs_expression(*arg2, autorefs)?.0),
        },
        BuiltinFunctionCall::Mul { arg1, arg2 } => BuiltinFunctionCall::Mul {
            arg1: Box::new(check_refs_expression(*arg1, autorefs)?.0),
            arg2: Box::new(check_refs_expression(*arg2, autorefs)?.0),
        },
        BuiltinFunctionCall::Div { arg1, arg2 } => BuiltinFunctionCall::Div {
            arg1: Box::new(check_refs_expression(*arg1, autorefs)?.0),
            arg2: Box::new(check_refs_expression(*arg2, autorefs)?.0),
        },
        BuiltinFunctionCall::Eq { arg1, arg2 } => BuiltinFunctionCall::Eq {
            arg1: Box::new(check_refs_expression(*arg1, autorefs)?.0),
            arg2: Box::new(check_refs_expression(*arg2, autorefs)?.0),
        },
        BuiltinFunctionCall::NotEq { arg1, arg2 } => BuiltinFunctionCall::NotEq {
            arg1: Box::new(check_refs_expression(*arg1, autorefs)?.0),
            arg2: Box::new(check_refs_expression(*arg2, autorefs)?.0),
        },
        BuiltinFunctionCall::Lesser { arg1, arg2 } => BuiltinFunctionCall::Lesser {
            arg1: Box::new(check_refs_expression(*arg1, autorefs)?.0),
            arg2: Box::new(check_refs_expression(*arg2, autorefs)?.0),
        },
        BuiltinFunctionCall::Greater { arg1, arg2 } => BuiltinFunctionCall::Greater {
            arg1: Box::new(check_refs_expression(*arg1, autorefs)?.0),
            arg2: Box::new(check_refs_expression(*arg2, autorefs)?.0),
        },
        BuiltinFunctionCall::LesserEq { arg1, arg2 } => BuiltinFunctionCall::LesserEq {
            arg1: Box::new(check_refs_expression(*arg1, autorefs)?.0),
            arg2: Box::new(check_refs_expression(*arg2, autorefs)?.0),
        },
        BuiltinFunctionCall::GreaterEq { arg1, arg2 } => BuiltinFunctionCall::GreaterEq {
            arg1: Box::new(check_refs_expression(*arg1, autorefs)?.0),
            arg2: Box::new(check_refs_expression(*arg2, autorefs)?.0),
        },
    };
    Ok(res)
}
