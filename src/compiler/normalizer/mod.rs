use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::lowerer::transform_function_name;
use crate::compiler::normalizer::ir::{IRBlock, IRConstant, IRExpression, IRFieldLabel, IRInstance, IRInstanceLabel, IROperator, IRPrimitiveType, IRStatement, IRStruct, IRStructLabel, IRType, IRTypeLabel, IRVariableLabel, IR};
use crate::compiler::normalizer::type_resolver::TypeResolver;
use crate::compiler::parser::ast::{
    ASTBlock, ASTExpression, ASTFunctionSignature, ASTOperator, ASTPrimitiveType, ASTStatement, ASTStructDeclaration, ASTType, Ast,
};
use std::collections::HashMap;

pub mod ir;
mod ir_debug;
mod type_resolver;
mod default_operator_map;
mod dsu;

fn operator_to_ir_operator(operator: ASTOperator) -> IROperator {
    match operator {
        ASTOperator::Plus => IROperator::Plus,
        ASTOperator::Mul => IROperator::Mul,
        ASTOperator::Div => IROperator::Div,
        ASTOperator::Equals => IROperator::Equals,
        ASTOperator::NotEquals => IROperator::NotEquals,
        ASTOperator::Greater => IROperator::Greater,
        ASTOperator::Less => IROperator::Lesser,
        ASTOperator::GreaterEquals => IROperator::GreaterOrEq,
        ASTOperator::LessEquals => IROperator::LesserOrEq,
        ASTOperator::Minus => IROperator::Minus,
    }
}

pub struct NormalizerState {
    type_resolver: TypeResolver,
    variables_name_map: HashMap<String, IRVariableLabel>,
    curr_var_label: IRVariableLabel,
    // key is (function name, number of arguments)
    functions_name_map: HashMap<(String, usize), Vec<(ASTFunctionSignature, ASTBlock)>>,
    curr_func_label: IRInstanceLabel,
    fields_name_map: HashMap<String, IRFieldLabel>,
    curr_field_label: IRFieldLabel,
    curr_func_vars: Vec<IRVariableLabel>,
    curr_func_ret_type: IRTypeLabel,
    has_ret_statement: bool,
    depth: i32,
    structs_name_map: HashMap<String, IRStructLabel>,
    structs_type_hints: Vec<Vec<ASTType>>,
    instance_cache: HashMap<String, Vec<(Vec<IRTypeLabel>, IRInstanceLabel)>>,
    template_types: HashMap<String, IRTypeLabel>,
}

impl NormalizerState {
    pub fn new_var_label(&mut self) -> IRVariableLabel {
        self.curr_var_label += 1;
        self.curr_var_label - 1
    }

    pub fn new_var(&mut self, name: &str) -> IRVariableLabel {
        let label = self.new_var_label();
        self.variables_name_map.insert(name.to_string(), label);
        label
    }

    pub fn new_field_label(&mut self) -> IRFieldLabel {
        self.curr_field_label += 1;
        self.curr_field_label - 1
    }

    pub fn new_field(&mut self, name: &str) -> IRFieldLabel {
        let label = self.new_field_label();
        self.fields_name_map.insert(name.to_string(), label);
        label
    }
}

pub fn normalize_ast(ast: Ast) -> CompilerResult<IR> {
    let mut ir = IR {
        structs: Vec::new(),
        instances: Vec::new(),
        types: Vec::new(),
        variable_types: Vec::new(),
        main_function: 0,
        autorefs: Vec::new(),
    };
    let mut state = NormalizerState {
        type_resolver: TypeResolver::new(),
        variables_name_map: HashMap::new(),
        curr_var_label: 0,
        functions_name_map: HashMap::new(),
        curr_func_label: 0,
        fields_name_map: HashMap::new(),
        curr_field_label: 0,
        curr_func_vars: Vec::new(),
        curr_func_ret_type: 0,
        has_ret_statement: false,
        depth: 0,
        structs_name_map: HashMap::new(),
        instance_cache: HashMap::new(),
        structs_type_hints: Vec::new(),
        template_types: HashMap::new(),
    };

    for (sig, block) in ast.functions {
        state.functions_name_map.entry((sig.name.clone(), sig.args.len())).or_default();

        if let Some(val) = state.functions_name_map.get_mut(&(sig.name.clone(), sig.args.len())) {
            val.push((sig, block));
        }
    }

    for structure in ast.structs {
        let name = structure.name.clone();
        let (ir_struct, type_hints) = normalize_struct(&mut state, structure);
        let label = ir.structs.len() as IRStructLabel;
        state.structs_type_hints.push(type_hints);
        state.structs_name_map.insert(name, label);
        ir.structs.push(ir_struct);
    }

    for (key, val) in &state.functions_name_map {
        if key.0 == transform_function_name("main".to_string()) && key.1 != 0 {
            return Err(CompilerError {
                message: "main function cannot have arguments".to_string(),
                position: Some(val[0].0.args[0].2.clone()),
            });
        }
    }

    if let Some(mut vec) = state.functions_name_map.get(&(transform_function_name("main".to_string()), 0)).cloned() {
        if vec.len() != 1 {
            return Err(CompilerError {
                message: "Multiple main functions found".to_string(),
                position: None,
            });
        }

        let (sig, block) = vec.pop().unwrap();

        ir.main_function = normalize_function(&mut state, &mut ir, sig, block, Vec::new())?;
    } else {
        return Err(CompilerError {
            message: "No main function found".to_string(),
            position: None,
        });
    }

    (ir.types, ir.autorefs) = state.type_resolver.gather_types()?;

    let main_ret = ir.instances[ir.main_function].ret_type;
    let main_ret = ir.types[main_ret].clone();

    if main_ret != IRType::Primitive(IRPrimitiveType::Void) {
        return Err(CompilerError {
            message: "Main function should not return any value".to_string(),
            position: None,
        });
    }

    // so that functions are in right order
    // if func0 is used inside func1 it should appear above func1 in generated c code
    ir.instances.reverse();

    Ok(ir)
}

fn normalize_struct(state: &mut NormalizerState, structure: ASTStructDeclaration) -> (IRStruct, Vec<ASTType>) {
    let mut ir_struct = IRStruct { fields: Vec::new() };
    let mut type_hints = Vec::new();

    for (field_name, field_type) in structure.fields {
        let label = if let Some(label) = state.fields_name_map.get(&field_name) {
            *label
        } else {
            let label = state.new_field(&field_name);
            state.fields_name_map.insert(field_name, label);
            label
        };
        ir_struct.fields.push(label);
        type_hints.push(field_type);
    }

    (ir_struct, type_hints)
}

fn find_matching_function(state: &mut NormalizerState, ir: &mut IR, function_name: String, function_arguments: Vec<IRTypeLabel>, pos: FilePosition) -> CompilerResult<(ASTFunctionSignature, ASTBlock)> {
    let candidates = if let Some(vec) = state.functions_name_map.get(&(function_name.clone(), function_arguments.len())).cloned() {
        vec
    } else {
        return Err(CompilerError {
            message: format!("Function {} does not exist.", &function_name[1..]),
            position: Some(pos),
        });
    };

    let mut matching = Vec::new();

    for (sign, block) in candidates {
        let old_resolver = state.type_resolver.clone();
        let old_template_types = state.template_types.clone();
        state.template_types.clear();
        let mut ok = true;

        for (template_arg, pos) in sign.template.clone() {
            state.template_types.insert(template_arg, state.type_resolver.new_type_label(pos));
        }

        for ((_, hint, _), typ) in sign.args.iter().zip(function_arguments.iter()) {
            let hint_typ = normalize_type(state, ir, hint.clone())?;

            if state.type_resolver.hint_equal(ir, *typ, hint_typ).is_err() {
                ok = false;
                continue;
            }
        }

        state.type_resolver = old_resolver;
        state.template_types = old_template_types;

        if ok {
            matching.push((sign, block));
        }
    }

    if matching.is_empty() {
        return Err(CompilerError {
            message: "No candidate found for this function call".to_string(),
            position: Some(pos),
        });
    }

    if matching.len() != 1 {
        return Err(CompilerError {
            message: "Multiple candidates found for this function call".to_string(),
            position: Some(pos),
        });
    }

    Ok(matching.pop().unwrap())
}

// returns (expression, expression type, is expression physical (assignable) value)
fn normalize_expression(state: &mut NormalizerState, ir: &mut IR, expression: ASTExpression) -> CompilerResult<(IRExpression, IRTypeLabel, bool)> {
    let type_label = state.type_resolver.new_type_label(expression.get_pos());

    let (expr, is_phys) = match expression {
        ASTExpression::Integer(x, _) => {
            state.type_resolver.hint_is(ir, type_label, IRPrimitiveType::I32)?;
            (IRExpression::Constant { constant: IRConstant::Int(x as i64) }, false)
        }
        ASTExpression::Float(x, _) => {
            state.type_resolver.hint_is(ir, type_label, IRPrimitiveType::F32)?;
            (IRExpression::Constant { constant: IRConstant::Float(x as f64) }, false)
        }
        ASTExpression::String(x, _) => {
            state.type_resolver.hint_is(ir, type_label, IRPrimitiveType::String)?;
            (IRExpression::Constant { constant: IRConstant::String(x.clone()) }, false)
        }
        ASTExpression::Boolean(x, _) => {
            state.type_resolver.hint_is(ir, type_label, IRPrimitiveType::Bool)?;
            (IRExpression::Constant { constant: IRConstant::Bool(x) }, false)
        }
        ASTExpression::Variable(name, pos) => {
            let label = *if let Some(label) = state.variables_name_map.get(&name) {
                label
            } else {
                return Err(CompilerError {
                    message: format!("Variable with name {name} not found."),
                    position: Some(pos),
                });
            };
            let var_type_label = ir.variable_types[label];
            state.type_resolver.hint_equal(ir, type_label, var_type_label)?;
            (IRExpression::Variable { variable_label: label }, true)
        }
        ASTExpression::Reference { expression, pos } => {
            let (expression, type_label2, is_phys) = normalize_expression(state, ir, *expression)?;

            if !is_phys {
                return Err(CompilerError {
                    message: "Cannot reference non-physical value.".to_string(),
                    position: Some(pos),
                });
            }

            state.type_resolver.hint_is_ref(ir, type_label2, type_label)?;
            (IRExpression::Reference { expression: Box::new(expression) }, false)
        }
        ASTExpression::Dereference { expression, pos: _ } => {
            let (expression, type_label2, _is_phys) = normalize_expression(state, ir, *expression)?;
            state.type_resolver.hint_is_ref(ir, type_label, type_label2)?;
            (IRExpression::Dereference { expression: Box::new(expression) }, true)
        }
        ASTExpression::FunctionCall { name, arguments, pos } => {
            let mut expr_types = Vec::new();
            let mut function_arguments = Vec::new();
            for expr in arguments {
                let (expr, type_label, _is_phys) = normalize_expression(state, ir, expr)?;
                expr_types.push(type_label);
                function_arguments.push(expr);
            }

            let (sig, block) = find_matching_function(state, ir, name, expr_types.clone(), pos)?;
            let function_label = normalize_function(state, ir, sig, block, expr_types)?;
            let ret_type = ir.instances[function_label].ret_type;
            state.type_resolver.hint_equal(ir, ret_type, type_label)?;

            (IRExpression::InstanceCall {
                instance_label: function_label,
                instance_arguments: function_arguments,
            }, false)
        }
        ASTExpression::StructInitialization { name, fields, pos: _ } => {
            let struct_label = state.structs_name_map[&name];
            let type_hints = state.structs_type_hints[struct_label].clone();

            let mut field_values = Vec::new();
            let mut fields_type_labels = Vec::new();
            for (arg, field_type) in fields.into_iter().zip(type_hints) {
                let (expr, typ, _is_phys) = normalize_expression(state, ir, arg)?;
                field_values.push(expr);
                fields_type_labels.push(typ);
                let type_hint = normalize_type(state, ir, field_type)?;
                state.type_resolver.hint_equal(ir, typ, type_hint)?;
            }

            let struct_expr = IRExpression::StructInitialization {
                struct_label,
                fields_type_labels: fields_type_labels.clone(),
                field_values,
            };
            state.type_resolver.hint_struct(ir, type_label, struct_label, fields_type_labels)?;

            (struct_expr, false)
        }
        ASTExpression::FieldAccess {
            expression,
            field_name,
            pos,
        } => {
            let (expression, type_label2, _is_phys) = normalize_expression(state, ir, *expression)?;
            let field_label = if let Some(x) = state.fields_name_map.get(&field_name) {
                *x
            } else {
                return Err(CompilerError {
                    message: format!("Unknown field {field_name}"),
                    position: Some(pos),
                })
            };
            state.type_resolver.hint_is_field(ir, type_label, type_label2, field_label)?;
            (IRExpression::FieldAccess {
                expression: Box::new(expression),
                field_label,
            }, true)
        }
        ASTExpression::MethodCall { .. } => {
            // should be eliminated by lowerer
            unreachable!()
        }
        ASTExpression::BinaryOperation {
            expression1,
            operator,
            expression2,
            pos: _,
        } => {
            let (expression1, type1_label, _is_phys) = normalize_expression(state, ir, *expression1)?;
            let (expression2, type2_label, _is_phys) = normalize_expression(state, ir, *expression2)?;
            let operator = operator_to_ir_operator(operator);
            state.type_resolver.hint_operator(ir, type1_label, type2_label, operator, type_label)?;
            (IRExpression::BinaryOperation {
                operator,
                expression1: Box::new(expression1),
                expression2: Box::new(expression2),
                type1_label,
                type2_label,
            }, false)
        }
        ASTExpression::AutoRef { expression } => {
            let (expression, type_label1, _is_phys) = normalize_expression(state, ir, *expression)?;
            let autoref_label = state.type_resolver.new_autoref_label(type_label, type_label1);

            state.type_resolver.hint_autoref(ir, type_label, type_label1)?;
            (IRExpression::AutoRef {
                autoref_label,
                expression: Box::new(expression),
            }, false)
        }
    };

    Ok((expr, type_label, is_phys))
}

fn primitive_type_to_ir_type(typ: ASTPrimitiveType) -> IRPrimitiveType {
    match typ {
        ASTPrimitiveType::I32 => IRPrimitiveType::I32,
        ASTPrimitiveType::I64 => IRPrimitiveType::I64,
        ASTPrimitiveType::F32 => IRPrimitiveType::F32,
        ASTPrimitiveType::F64 => IRPrimitiveType::F64,
        ASTPrimitiveType::Bool => IRPrimitiveType::Bool,
        ASTPrimitiveType::String => IRPrimitiveType::String,
        ASTPrimitiveType::Void => IRPrimitiveType::Void,
    }
}

fn normalize_type(state: &mut NormalizerState, ir: &mut IR, typ: ASTType) -> CompilerResult<IRTypeLabel> {
    let type_label = state.type_resolver.new_type_label(typ.get_pos());
    match typ {
        ASTType::Any(_) => {}
        ASTType::Primitive(typ, _) => {
            let typ = primitive_type_to_ir_type(typ);
            state.type_resolver.hint_is(ir, type_label, typ)?;
        }
        ASTType::Struct(name, pos) => {
            if let Some(label) = state.template_types.get(&name).cloned() {
                state.type_resolver.hint_equal(ir, label, type_label)?;
            } else if let Some(struct_label) = state.structs_name_map.get(&name).cloned() {
                let mut args = Vec::new();
                for _ in &ir.structs[struct_label].fields {
                    args.push(state.type_resolver.new_type_label(pos.clone()));
                }

                state.type_resolver.hint_struct(ir, type_label, struct_label, args)?;
            } else {
                return Err(CompilerError {
                    message: format!("Unknown type: {name}"),
                    position: Some(pos),
                })
            };
        }
        ASTType::Reference(typ, _) => {
            let type_label2 = normalize_type(state, ir, *typ)?;
            state.type_resolver.hint_is_ref(ir, type_label2, type_label)?;
        }
    }
    Ok(type_label)
}

fn normalize_block(state: &mut NormalizerState, ir: &mut IR, block: ASTBlock) -> CompilerResult<IRBlock> {
    let mut res = IRBlock { statements: Vec::new() };
    let prev_vars = state.variables_name_map.clone();

    for statement in block.children {
        match statement {
            ASTStatement::Assignment { assign_to, value, pos } => {
                // if an unknown variable is assigned, create it
                if let ASTExpression::Variable(name, pos) = &assign_to
                    && !state.variables_name_map.contains_key(name)
                {
                    let label = state.new_var(name);
                    state.curr_func_vars.push(label);
                    ir.variable_types.push(state.type_resolver.new_type_label(pos.clone()));
                }

                let (assign_to, type_label1, is_phys) = normalize_expression(state, ir, assign_to)?;
                let (value, type_label2, _is_phys) = normalize_expression(state, ir, value)?;
                state.type_resolver.hint_equal(ir, type_label1, type_label2)?;

                if !is_phys {
                    return Err(CompilerError {
                        message: "Left hand side is non-assignable".to_string(),
                        position: Some(pos),
                    });
                }

                res.statements.push(IRStatement::Assignment { assign_to, value });
            }
            ASTStatement::AssignmentOperator { .. } => unreachable!(), // lowerer took care of that
            ASTStatement::AssignmentIncrement { .. } => unreachable!(),
            ASTStatement::AssignmentDecrement { .. } => unreachable!(),

            ASTStatement::Block { block } => {
                let block = normalize_block(state, ir, block)?;
                res.statements.push(IRStatement::Block { block });
            }
            ASTStatement::Expression { expression } => {
                res.statements.push(IRStatement::Expression {
                    expr: normalize_expression(state, ir, expression)?.0,
                });
            }
            ASTStatement::Print { values } => {
                let mut vals = values;
                vals.push(ASTExpression::String("\n".to_string(), FilePosition::unknown()));
                for val in vals {
                    let (expr, type_label, _is_phys) = normalize_expression(state, ir, val)?;
                    res.statements.push(IRStatement::Print { expr, type_label });
                }
            }
            ASTStatement::Return { return_value, pos: _ } => {
                state.has_ret_statement = true;
                let st = if let Some(expr) = return_value {
                    let (expr, type_label, _is_phys) = normalize_expression(state, ir, expr)?;
                    state.type_resolver.hint_equal(ir, state.curr_func_ret_type, type_label)?;
                    IRStatement::Return { return_value: Some(expr) }
                } else {
                    state.type_resolver.hint_is(ir, state.curr_func_ret_type, IRPrimitiveType::Void)?;
                    IRStatement::Return { return_value: None }
                };
                res.statements.push(st);
            }
            ASTStatement::If { condition, block, else_block } => {
                let (condition, type_label, _is_phys) = normalize_expression(state, ir, condition)?;
                state.type_resolver.hint_is(ir, type_label, IRPrimitiveType::Bool)?;
                let block = normalize_block(state, ir, block)?;
                let else_block = if let Some(else_block) = else_block {
                    Some(normalize_block(state, ir, else_block)?)
                } else {
                    None
                };
                res.statements.push(IRStatement::If { condition, block, else_block });
            }
            ASTStatement::While { condition, block } => {
                let (condition, type_label, _is_phys) = normalize_expression(state, ir, condition)?;
                state.type_resolver.hint_is(ir, type_label, IRPrimitiveType::Bool)?;
                let block = normalize_block(state, ir, block)?;
                res.statements.push(IRStatement::While { condition, block });
            }
        }
    }

    state.variables_name_map = prev_vars;

    Ok(res)
}

fn normalize_function(state: &mut NormalizerState, ir: &mut IR, sign: ASTFunctionSignature, block: ASTBlock, arg_types: Vec<IRTypeLabel>) -> CompilerResult<IRInstanceLabel> {
    assert_eq!(arg_types.len(), sign.args.len());

    if let Some(cache) = state.instance_cache.get(&sign.name) {
        for (types, label) in cache {
            if types.len() != arg_types.len() {
                continue;
            }

            let mut ok = true;
            for (t1, t2) in types.iter().zip(&arg_types) {
                if !state.type_resolver.are_equal(*t1, *t2) {
                    ok = false;
                    break;
                }
            }

            if ok {
                return Ok(*label);
            }
        }
    }

    let prev_vars = state.variables_name_map.clone();
    let prev_func_vars = state.curr_func_vars.clone();
    let prev_func_ret_type = state.curr_func_ret_type;
    let prev_has_ret_statement = state.has_ret_statement;

    const RECURSION_LIMIT: i32 = 100;
    if state.depth == RECURSION_LIMIT {
        return Err(CompilerError {
            message: format!("This function is in the {RECURSION_LIMIT}-th recursive call \
            in the normalization phase. Make sure argument types are more \
            explicit and the function does not generate infinitely many functions recursively."),
            position: Some(sign.pos),
        });
    }

    state.depth += 1;
    state.curr_func_vars = Vec::new();
    state.curr_func_ret_type = state.type_resolver.new_type_label(sign.pos);
    state.has_ret_statement = false;

    let label = state.curr_func_label;
    state.curr_func_label += 1;

    for (template_arg, pos) in sign.template {
        state.template_types.insert(template_arg, state.type_resolver.new_type_label(pos));
    }

    let mut arguments = Vec::new();
    for ((arg, type_hint, _pos), arg_type) in sign.args.into_iter().zip(arg_types.clone()) {
        let hint_label = normalize_type(state, ir, type_hint)?;
        let label = state.new_var(&arg);
        arguments.push(label);
        ir.variable_types.push(arg_type);
        state.type_resolver.hint_equal(ir, hint_label, arg_type)?;
    }

    ir.instances.push(IRInstance {
        arguments,
        variables: Vec::new(),
        ret_type: state.curr_func_ret_type,
        block: IRBlock { statements: Vec::new() },
        label,
    });

    if !state.instance_cache.contains_key(&sign.name) {
        state.instance_cache.insert(sign.name.clone(), Vec::new());
    }

    state.instance_cache.get_mut(&sign.name).unwrap().push((arg_types, label));

    ir.instances[label].block = normalize_block(state, ir, block)?;
    ir.instances[label].variables = state.curr_func_vars.clone();

    if !state.has_ret_statement {
        state.type_resolver.hint_is(ir, state.curr_func_ret_type, IRPrimitiveType::Void)?;
    }

    state.variables_name_map = prev_vars;
    state.curr_func_vars = prev_func_vars;
    state.curr_func_ret_type = prev_func_ret_type;
    state.has_ret_statement = prev_has_ret_statement;
    state.template_types.clear();
    state.depth -= 1;

    Ok(label)
}
