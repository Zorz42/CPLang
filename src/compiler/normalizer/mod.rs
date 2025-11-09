use std::collections::HashMap;
use crate::compiler::error::CompilerResult;
use crate::compiler::lowerer::transform_function_name;
use crate::compiler::normalizer::ir::{IRAutoRefLabel, IRBlock, IRConstant, IRExpression, IRFieldLabel, IRFunction, IRFunctionLabel, IROperator, IRPrimitiveType, IRStatement, IRStruct, IRStructLabel, IRTypeLabel, IRVariableLabel, IR};
use crate::compiler::normalizer::type_resolver::{resolve_types, IRTypeHint};
use crate::compiler::parser::ast::{Assignment, ASTBlock, ASTExpression, ASTFunctionSignature, ASTOperator, ASTPrimitiveType, ASTStatement, StructDeclaration, ASTType, AST};

pub mod ir;
mod type_resolver;
mod ir_debug;

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
    variables_name_map: HashMap<String, IRVariableLabel>,
    curr_var_label: IRVariableLabel,
    functions_name_map: HashMap<String, (ASTFunctionSignature, ASTBlock)>,
    curr_func_label: IRFunctionLabel,
    fields_name_map: HashMap<String, IRFieldLabel>,
    curr_field_label: IRFieldLabel,
    curr_type_label: IRTypeLabel,
    type_hints: Vec<IRTypeHint>,
    curr_func_vars: Vec<IRVariableLabel>,
    curr_func_ret_type: IRTypeLabel,
    has_ret_statement: bool,
    depth: i32,
    structs_name_map: HashMap<String, IRStructLabel>,
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

    pub fn new_type_label(&mut self) -> IRTypeLabel {
        self.curr_type_label += 1;
        self.curr_type_label - 1
    }
}

pub fn normalize_ast(ast: AST) -> CompilerResult<IR> {
    let mut ir = IR {
        structs: Vec::new(),
        functions: Vec::new(),
        types: Vec::new(),
        variable_types: Vec::new(),
        main_function: 0,
        autorefs: Vec::new(),
    };
    let mut state = NormalizerState {
        variables_name_map: HashMap::new(),
        curr_var_label: 0,
        functions_name_map: HashMap::new(),
        curr_func_label: 0,
        fields_name_map: HashMap::new(),
        curr_field_label: 0,
        curr_type_label: 0,
        type_hints: Vec::new(),
        curr_func_vars: Vec::new(),
        curr_func_ret_type: 0,
        has_ret_statement: false,
        depth: 0,
        structs_name_map: HashMap::new(),
    };

    for (sig, block) in ast.functions {
        state.functions_name_map.insert(sig.name.clone(), (sig, block));
    }

    for structure in ast.structs {
        let name = structure.name.clone();
        let ir_struct = normalize_struct(&mut state, structure);
        let label = ir.structs.len() as IRStructLabel;
        state.structs_name_map.insert(name, label);
        ir.structs.push(ir_struct);
    }

    if let Some((sig, block)) = state.functions_name_map.get(&transform_function_name("main".to_string())).cloned() {
        if !sig.args.is_empty() {
            panic!();
        }

        ir.main_function = normalize_function(&mut state, &mut ir, sig, block, Vec::new());
    } else {
        panic!();
    }

    resolve_types(&mut ir, state.curr_type_label, state.type_hints);

    Ok(ir)
}

fn normalize_struct(state: &mut NormalizerState, structure: StructDeclaration) -> IRStruct{
    let mut ir_struct = IRStruct {
        fields: Vec::new(),
    };

    for name in structure.fields {
        let label =
            if let Some(label) = state.fields_name_map.get(&name) {
                *label
            } else {
                let label = state.new_field(&name);
                state.fields_name_map.insert(name, label);
                label
            };
        ir_struct.fields.push(label);
    }

    ir_struct

}

fn normalize_expression(state: &mut NormalizerState, ir: &mut IR, expression: ASTExpression) -> (IRExpression, IRTypeLabel) {
    let type_label = state.new_type_label();

    let expr = match expression {
        ASTExpression::Integer(x) => {
            state.type_hints.push(IRTypeHint::Is(type_label, IRPrimitiveType::I32));
            IRExpression::Constant(IRConstant::Int(x as i64))
        }
        ASTExpression::Float(x) => {
            state.type_hints.push(IRTypeHint::Is(type_label, IRPrimitiveType::F32));
            IRExpression::Constant(IRConstant::Float(x as f64))
        }
        ASTExpression::String(x) => {
            state.type_hints.push(IRTypeHint::Is(type_label, IRPrimitiveType::String));
            IRExpression::Constant(IRConstant::String(x.clone()))
        }
        ASTExpression::Boolean(x) => {
            state.type_hints.push(IRTypeHint::Is(type_label, IRPrimitiveType::Bool));
            IRExpression::Constant(IRConstant::Bool(x))
        }
        ASTExpression::Variable(name, _pos) => {
            let label = state.variables_name_map[&name];
            let var_type_label = ir.variable_types[label];
            state.type_hints.push(IRTypeHint::Equal(type_label, var_type_label));
            IRExpression::Variable(label)
        }
        ASTExpression::Reference(expr, _pos) => {
            let (expr, type_label2) = normalize_expression(state, ir, *expr);
            state.type_hints.push(IRTypeHint::IsRef(type_label, type_label2));
            IRExpression::Reference(Box::new(expr))
        }
        ASTExpression::Dereference(expr, _pos) => {
            let (expr, type_label2) = normalize_expression(state, ir, *expr);
            state.type_hints.push(IRTypeHint::IsRef(type_label2, type_label));
            IRExpression::Dereference(Box::new(expr))
        }
        ASTExpression::FunctionCall(name, args) => {
            let mut expr_types = Vec::new();
            let mut ir_args = Vec::new();
            for expr in args {
                let (expr, type_label) = normalize_expression(state, ir, expr);
                expr_types.push(type_label);
                ir_args.push(expr);
            }

            let (sig, block) = state.functions_name_map.get(&name).cloned().unwrap();
            let func_label = normalize_function(state, ir, sig, block, expr_types);
            let ret_type = ir.functions[func_label].ret_type;
            state.type_hints.push(IRTypeHint::Equal(ret_type, type_label));

            IRExpression::FunctionCall(func_label, ir_args)
        }
        ASTExpression::StructInitialization(name, args) => {
            let struct_label = state.structs_name_map[&name].clone();

            let mut args_exprs = Vec::new();
            let mut args_types = Vec::new();
            for arg in args {
                let (expr, typ) = normalize_expression(state, ir, arg);
                args_exprs.push(expr);
                args_types.push(typ);
            }

            let struct_expr = IRExpression::StructInitialization(struct_label, args_types.clone(), args_exprs);
            state.type_hints.push(IRTypeHint::Struct(type_label, struct_label, args_types));

            struct_expr
        }
        ASTExpression::FieldAccess(expr, field, _pos) => {
            let (expr, type_label2) = normalize_expression(state, ir, *expr);
            let field_label = state.fields_name_map[&field];
            state.type_hints.push(IRTypeHint::IsField(type_label, type_label2, field_label));
            IRExpression::FieldAccess(Box::new(expr), field_label)
        }
        ASTExpression::MethodCall(..) => {
            // should be eliminated by lowerer
            unreachable!()
        }
        ASTExpression::BinaryOperation(expr1, op, expr2, _pos) => {
            let (expr1, type_label1) = normalize_expression(state, ir, *expr1);
            let (expr2, type_label2) = normalize_expression(state, ir, *expr2);
            let op = operator_to_ir_operator(op);
            state.type_hints.push(IRTypeHint::Operator(
                type_label, type_label1, op, type_label2
            ));
            IRExpression::BinaryOperation(op, Box::new((expr1, type_label1, expr2, type_label2)))
        }
        ASTExpression::AutoRef(expr) => {
            let (expr, type_label1) = normalize_expression(state, ir, *expr);
            let autoref_label = ir.autorefs.len() as IRAutoRefLabel;
            ir.autorefs.push(0);
            
            state.type_hints.push(IRTypeHint::AutoRef(autoref_label, type_label, type_label1));
            IRExpression::AutoRef(autoref_label, Box::new(expr))
        }
    };

    (expr, type_label)
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

fn normalize_type(state: &mut NormalizerState, ir: &mut IR, typ: ASTType) -> IRTypeLabel {
    let type_label = state.new_type_label();
    match typ {
        ASTType::Any => {}
        ASTType::Primitive(typ) => {
            let typ = primitive_type_to_ir_type(typ);
            state.type_hints.push(IRTypeHint::Is(type_label, typ));
        }
        ASTType::Struct(_name) => {
            // TODO: hint that the type is actually this struct - will enable stronger type deduction
            // for now it only hints its not a reference
            state.type_hints.push(IRTypeHint::IsPhys(type_label));
        }
        ASTType::Reference(typ) => {
            let type_label2 = normalize_type(state, ir, *typ);
            state.type_hints.push(IRTypeHint::IsRef(type_label, type_label2));
        }
    }
    type_label
}

fn normalize_block(state: &mut NormalizerState, ir: &mut IR, block: ASTBlock) -> IRBlock {
    let mut res = IRBlock {
        statements: Vec::new(),
    };
    let prev_vars = state.variables_name_map.clone();

    for statement in block.children {
        match statement {
            ASTStatement::Assignment(declaration) => {
                let (assign_to, value, _pos) = match declaration {
                    Assignment::Assign(assign_to, value, pos) => (assign_to, value, pos),
                    _ => unreachable!(), // lowerer took care of that
                };

                // if an unknown variable is assigned, create it
                if let ASTExpression::Variable(name, _) = &assign_to && !state.variables_name_map.contains_key(name) {
                    let label = state.new_var(name);
                    state.curr_func_vars.push(label);
                    ir.variable_types.push(state.new_type_label());
                }

                let (assign_to, type_label1) = normalize_expression(state, ir, assign_to);
                let (value, type_label2) = normalize_expression(state, ir, value);
                state.type_hints.push(IRTypeHint::Equal(type_label1, type_label2));

                res.statements.push(IRStatement::Assignment(assign_to, value));
            }
            ASTStatement::Block(block) => {
                let block = normalize_block(state, ir, block);
                res.statements.push(IRStatement::Block(block));
            }
            ASTStatement::Expression(expr) => {
                res.statements.push(IRStatement::Expression(normalize_expression(state, ir, expr).0));
            }
            ASTStatement::Print(statement) => {
                let mut vals = statement.values;
                vals.push(ASTExpression::String("\n".to_string()));
                for val in vals {
                    let (expr, type_label) = normalize_expression(state, ir, val);
                    res.statements.push(IRStatement::Print(expr, type_label));
                }
            }
            ASTStatement::Return(expr, _pos) => {
                state.has_ret_statement = true;
                let st = if let Some(expr) = expr {
                    let (expr, type_label) = normalize_expression(state, ir, expr);
                    state.type_hints.push(IRTypeHint::Equal(state.curr_func_ret_type, type_label));
                    IRStatement::Return(Some(expr))
                } else {
                    state.type_hints.push(IRTypeHint::Is(state.curr_func_ret_type, IRPrimitiveType::Void));
                    IRStatement::Return(None)
                };
                res.statements.push(st);
            }
            ASTStatement::If(statement) => {
                let (cond, type_label) = normalize_expression(state, ir, statement.condition);
                state.type_hints.push(IRTypeHint::Is(type_label, IRPrimitiveType::Bool));
                let block = normalize_block(state, ir, statement.block);
                let else_block = statement.else_block.map(|block| normalize_block(state, ir, block));
                res.statements.push(IRStatement::If(cond, block, else_block));
            }
            ASTStatement::While(statement) => {
                let (cond, type_label) = normalize_expression(state, ir, statement.condition);
                state.type_hints.push(IRTypeHint::Is(type_label, IRPrimitiveType::Bool));
                let block = normalize_block(state, ir, statement.block);
                res.statements.push(IRStatement::While(cond, block));
            }
        }
    }

    state.variables_name_map = prev_vars;

    res
}

fn normalize_function(state: &mut NormalizerState, ir: &mut IR, sign: ASTFunctionSignature, block: ASTBlock, arg_types: Vec<IRTypeLabel>) -> IRFunctionLabel {
    assert_eq!(arg_types.len(), sign.args.len());

    let prev_vars = state.variables_name_map.clone();
    let prev_func_vars = state.curr_func_vars.clone();
    let prev_func_ret_type = state.curr_func_ret_type.clone();
    let prev_has_ret_statement = state.has_ret_statement;

    // avoid infinite recursion
    if state.depth == 50 {
        panic!("Recursion too deep");
    }

    state.depth += 1;
    state.curr_func_vars = Vec::new();
    state.curr_func_ret_type = state.new_type_label();
    state.has_ret_statement = false;

    let mut arguments = Vec::new();
    for ((arg, type_hint), arg_type) in sign.args.into_iter().zip(arg_types) {
        let hint_label = normalize_type(state, ir, type_hint);
        let label = state.new_var(&arg);
        arguments.push(label);
        ir.variable_types.push(arg_type);
        state.type_hints.push(IRTypeHint::Equal(hint_label, arg_type));
    }
    let block = normalize_block(state, ir, block);
    let label = state.curr_func_label;
    state.curr_func_label += 1;

    if !state.has_ret_statement {
        state.type_hints.push(IRTypeHint::Is(state.curr_func_ret_type, IRPrimitiveType::Void));
    }

    ir.functions.push(IRFunction {
        arguments,
        variables: state.curr_func_vars.clone(),
        ret_type: state.curr_func_ret_type,
        block,
        label,
    });

    state.variables_name_map = prev_vars;
    state.curr_func_vars = prev_func_vars;
    state.curr_func_ret_type = prev_func_ret_type;
    state.has_ret_statement = prev_has_ret_statement;
    state.depth -= 1;

    label
}