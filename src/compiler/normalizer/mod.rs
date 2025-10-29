use std::collections::HashMap;
use crate::compiler::error::CompilerResult;
use crate::compiler::normalizer::ir::{IRBlock, IRConstant, IRExpression, IRFieldLabel, IRFunction, IRFunctionLabel, IROperator, IRStatement, IRStruct, IRTypeLabel, IRVariableLabel, IR};
use crate::compiler::parser::{Statement, AST};
use crate::compiler::parser::block::Block;
use crate::compiler::parser::expression::{Expression, Operator};
use crate::compiler::parser::function::FunctionSignature;

mod ir;

fn operator_to_ir_operator(operator: Operator) -> IROperator {
    match operator {
        Operator::Plus => IROperator::Plus,
        Operator::Mul => IROperator::Times,
        Operator::Div => IROperator::Divide,
        Operator::Equals => IROperator::Equals,
        Operator::NotEquals => IROperator::NotEquals,
        Operator::Greater => IROperator::Greater,
        Operator::Less => IROperator::Lesser,
        Operator::GreaterEquals => IROperator::GreaterOrEq,
        Operator::LessEquals => IROperator::LesserOrEq,
        Operator::Minus => IROperator::Minus,
    }
}

pub struct NormalizerState {
    variables: HashMap<String, IRVariableLabel>,
    curr_var_label: IRVariableLabel,
    functions: HashMap<String, (FunctionSignature, Block)>,
    curr_func_label: IRFunctionLabel,
    fields: HashMap<String, IRFieldLabel>,
    curr_field_label: IRFieldLabel,
    curr_type_label: IRTypeLabel,
}

impl NormalizerState {
    pub fn new_var_label(&mut self) -> IRVariableLabel {
        self.curr_var_label += 1;
        self.curr_var_label - 1
    }

    pub fn new_var(&mut self, name: &str) -> IRVariableLabel {
        let label = self.new_var_label();
        self.variables.insert(name.to_string(), label);
        label
    }

    pub fn new_field_label(&mut self) -> IRFieldLabel {
        self.curr_field_label += 1;
        self.curr_field_label - 1
    }

    pub fn new_field(&mut self, name: &str) -> IRFieldLabel {
        let label = self.new_field_label();
        self.fields.insert(name.to_string(), label);
        label
    }
}

pub fn normalize_ast(ast: AST) -> CompilerResult<IR> {
    let mut res = IR {
        structs: Vec::new(),
        functions: Vec::new(),
        main_function: 0,
    };
    let mut state = NormalizerState {
        variables: HashMap::new(),
        curr_var_label: 0,
        functions: HashMap::new(),
        curr_func_label: 0,
        fields: HashMap::new(),
        curr_field_label: 0,
        curr_type_label: 0,
    };


    for (sig, block) in ast.functions {
        state.functions.insert(sig.name.clone(), (sig, block));
    }

    if let Some((sig, block)) = state.functions.get("main").cloned() {
        if !sig.args.is_empty() {
            panic!();
        }

        normalize_function(&mut state, &mut res, sig, block, Vec::new());
    } else {
        panic!();
    }

    Ok(res)
}

fn normalize_expression(state: &mut NormalizerState, ir: &mut IR, expression: Expression) -> (IRExpression, IRTypeLabel) {
    let type_label = state.curr_type_label;
    state.curr_type_label += 1;

    let expr = match expression {
        Expression::Integer(x) => {
            IRExpression::Constant(IRConstant::Int(x as i64))
        }
        Expression::Float(x) => {
            IRExpression::Constant(IRConstant::Float(x as f64))
        }
        Expression::String(x) => {
            IRExpression::Constant(IRConstant::String(x.clone()))
        }
        Expression::Boolean(x) => {
            IRExpression::Constant(IRConstant::Bool(x))
        }
        Expression::Variable(name, _pos) => {
            let label = state.variables[&name];
            IRExpression::Variable(label)
        }
        Expression::Reference(expr, _pos) => {
            let (expr, _type_label) = normalize_expression(state, ir, *expr);
            IRExpression::Reference(Box::new(expr))
        }
        Expression::Dereference(expr, _pos) => {
            let (expr, _type_label) = normalize_expression(state, ir, *expr);
            IRExpression::Dereference(Box::new(expr))
        }
        Expression::FunctionCall(name, args) => {
            let mut expr_types = Vec::new();
            let mut ir_args = Vec::new();
            for expr in args {
                let (expr, type_label) = normalize_expression(state, ir, expr);
                expr_types.push(type_label);
                ir_args.push(expr);
            }

            let (sig, block) = state.functions.get(&name).cloned().unwrap();
            let func_label = normalize_function(state, ir, sig, block, expr_types);

            IRExpression::FunctionCall(func_label, ir_args)
        }
        Expression::StructInitialization(name, args) => {
            todo!()
        }
        Expression::FieldAccess(expr, field, _pos) => {
            let (expr, _type_label) = normalize_expression(state, ir, *expr);
            IRExpression::FieldAccess(Box::new(expr), state.fields[&field])
        }
        Expression::MethodCall(expr, _pos, name, args) => {
            todo!()
        }
        Expression::BinaryOperation(expr1, op, expr2, _pos) => {
            let (expr1, _type_label) = normalize_expression(state, ir, *expr1);
            let (expr2, _type_label) = normalize_expression(state, ir, *expr2);
            let op = operator_to_ir_operator(op);
            IRExpression::BinaryOperation(op, Box::new((expr1, expr2)))
        }
    };

    (expr, type_label)
}

fn normalize_block(state: &mut NormalizerState, ir: &mut IR, block: Block) -> IRBlock {
    let mut res = IRBlock {
        variables: Vec::new(),
        statements: Vec::new(),
    };
    let prev_vars = state.variables.clone();

    for statement in block.children {
        match statement {
            Statement::Assignment(declaration) => {
                // if an unknown variable is assigned, create it
                if let Expression::Variable(name, _) = &declaration.assign_to && !state.variables.contains_key(name) {
                    let label = state.new_var(name);
                    res.variables.push(label);
                }

                let (assign_to, _type_label) = normalize_expression(state, ir, declaration.assign_to);
                let (value, _type_label) = normalize_expression(state, ir, declaration.value);


                res.statements.push(IRStatement::Assignment(assign_to, value));
            }
            Statement::Block(block) => {
                let block = normalize_block(state, ir, block);
                res.statements.push(IRStatement::Block(block));
            }
            Statement::Expression(expr) => {
                res.statements.push(IRStatement::Expression(normalize_expression(state, ir, expr).0));
            }
            Statement::Print(statement) => {
                for val in statement.values {
                    res.statements.push(IRStatement::Print(normalize_expression(state, ir, val).0));
                }
            }
            Statement::Return(expr, _pos) => {
                let expr = expr.map(|x| normalize_expression(state, ir, x).0);
                res.statements.push(IRStatement::Return(expr));
            }
            Statement::If(statement) => {
                let (cond, _type_label) = normalize_expression(state, ir, statement.condition);
                let block = normalize_block(state, ir, statement.block);
                res.statements.push(IRStatement::If(cond, block));
            }
            Statement::While(statement) => {
                let (cond, _type_label) = normalize_expression(state, ir, statement.condition);
                let block = normalize_block(state, ir, statement.block);
                res.statements.push(IRStatement::If(cond, block));
            }
        }
    }

    state.variables = prev_vars;

    res
}

fn normalize_function(state: &mut NormalizerState, ir: &mut IR, sign: FunctionSignature, block: Block, arg_types: Vec<IRTypeLabel>) -> IRFunctionLabel {
    assert_eq!(arg_types.len(), sign.args.len());

    let prev_vars = state.variables.clone();
    let arguments = sign.args.iter().map(|arg| state.new_var(arg)).collect::<Vec<_>>();
    let block = normalize_block(state, ir, block);
    let label = ir.functions.len() as IRFunctionLabel;
    ir.functions.push(IRFunction {
        arguments,
        block,
    });
    state.variables = prev_vars;
    label
}