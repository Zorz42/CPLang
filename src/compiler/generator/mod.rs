use std::collections::HashMap;
use crate::compiler::normalizer::ir::{IRBlock, IRConstant, IRExpression, IRFunction, IRFunctionLabel, IROperator, IRPrimitiveType, IRStatement, IRType, IRTypeLabel, IRVariableLabel, IR};

struct GeneratorContext {
    types: Vec<IRType>,
    var_types: Vec<IRTypeLabel>,
    operators: HashMap<(IRType, IROperator, IRType), Box<dyn Fn(String, String) -> String>>,
}

fn init_default_operators() -> HashMap<(IRType, IROperator, IRType), Box<dyn Fn(String, String) -> String>> {
    let mut res: HashMap<(IRType, IROperator, IRType), Box<dyn Fn(String, String) -> String>> = HashMap::new();

    let c_plus = |a: String, b: String| -> String {
        format!("{a} + {b}")
    };
    res.insert((IRType::Primitive(IRPrimitiveType::I32), IROperator::Plus, IRType::Primitive(IRPrimitiveType::I32)), Box::new(c_plus));

    res
}

pub fn generate_code(ir: IR) -> String {
    let mut code = "#include<stdio.h>\n#include<stdlib.h>\n\n".to_owned();

    let ctx = GeneratorContext {
        types: ir.types,
        var_types: ir.variable_types,
        operators: init_default_operators(),
    };

    for func in ir.functions {
        code += &gen_function(&ctx, func);
    }

    code
}

fn gen_primitive_type(typ: IRPrimitiveType) -> String {
    match typ {
        IRPrimitiveType::I32 => "int",
        IRPrimitiveType::I64 => "long",
        IRPrimitiveType::F32 => "float",
        IRPrimitiveType::F64 => "double",
        IRPrimitiveType::Bool => "int",
        IRPrimitiveType::String => "char*",
        IRPrimitiveType::Void => "void",
    }.to_owned()
}

fn gen_type(typ: IRType) -> String {
    match typ {
        IRType::Primitive(typ) => gen_primitive_type(typ),
        IRType::Reference(_) => {
            todo!()
        }
        IRType::Tuple(_) => {
            todo!()
        }
        IRType::Enum(_) => {
            todo!()
        }
        IRType::Struct(_) => {
            todo!()
        }
    }
}

fn gen_function_label(func: IRFunctionLabel) -> String {
    format!("func{}", func)
}

fn gen_variable_label(func: IRVariableLabel) -> String {
    format!("var{}", func)
}

fn type_to_printf_format(typ: &IRType) -> &'static str {
    match typ {
        IRType::Primitive(typ) => {
            match typ {
                IRPrimitiveType::I32 => "d",
                IRPrimitiveType::I64 => "ld",
                IRPrimitiveType::F32 => "f",
                IRPrimitiveType::F64 => "lf",
                IRPrimitiveType::Bool => "d",
                IRPrimitiveType::String => "s",
                IRPrimitiveType::Void => unreachable!(),
            }
        }
        IRType::Reference(typ) => type_to_printf_format(typ),
        IRType::Tuple(_) => todo!(),
        IRType::Enum(_) => todo!(),
        IRType::Struct(_) => todo!(),
    }
}

fn gen_expression(ctx: &GeneratorContext, expression: IRExpression) -> String {
    match expression {
        IRExpression::BinaryOperation(op, ex_pair) => {
            let (expr1, type1, expr2, type2) = *ex_pair;
            let code1 = gen_expression(ctx, expr1);
            let code2 = gen_expression(ctx, expr2);
            let typ1 = ctx.types[type1].clone();
            let typ2 = ctx.types[type2].clone();
            format!("({})", ctx.operators[&(typ1, op, typ2)](code1, code2))
        }
        IRExpression::Constant(x) => {
            match x {
                IRConstant::String(x) => format!("\"{x}\""),
                IRConstant::Int(x) => format!("{x}"),
                IRConstant::Float(x) => format!("{x}"),
                IRConstant::Bool(x) => (if x { "1" } else { "0" }).to_string()
            }
        }
        IRExpression::FunctionCall(func, args) => {
            let mut args_code = String::new();

            for arg in args {
                args_code += &gen_expression(ctx, arg);
                args_code += ",";
            }
            // pop the last "," if possible
            args_code.pop();

            format!("{}({})", gen_function_label(func), args_code)
        }
        IRExpression::FieldAccess(_, _) => { todo!() }
        IRExpression::Dereference(_) => { todo!() }
        IRExpression::StructInitialization(_, _) => { todo!() }
        IRExpression::Reference(_) => { todo!() }
        IRExpression::Variable(var) => {
            gen_variable_label(var)
        }
    }
}

fn gen_block(ctx: &GeneratorContext, block: IRBlock) -> String {
    let mut code = String::new();

    for statement in block.statements {
        let s_code = match statement {
            IRStatement::Block(_) => todo!(),
            IRStatement::If(_, _) => todo!(),
            IRStatement::While(_, _) => todo!(),
            IRStatement::Expression(expr) => gen_expression(ctx, expr),
            IRStatement::Print(expr, type_label) => {
                let typ = ctx.types[type_label].clone();
                if typ == IRType::Primitive(IRPrimitiveType::Void) {
                    String::new()
                } else {
                    format!("printf(\"%{}\",{});", type_to_printf_format(&typ), gen_expression(ctx, expr))
                }
            }
            IRStatement::Return(expr) => {
                if let Some(expr) = expr {
                    format!("return {};", gen_expression(ctx, expr))
                } else {
                    "return;".to_string()
                }
            }
            IRStatement::Assignment(assign_to, expr) => {
                format!("{} = {};", gen_expression(ctx, assign_to), gen_expression(ctx, expr))
            }
        };
        code += &s_code;
        code += "\n";
    }

    code
}

fn gen_function(ctx: &GeneratorContext, func: IRFunction) -> String {
    let mut args = String::new();
    for arg in func.arguments {
        let typ = ctx.types[ctx.var_types[arg]].clone();
        args += &format!("{} {},", gen_type(typ), gen_variable_label(arg));
    }
    // pop the last "," if it exists
    args.pop();

    let mut code = format!("{} {}({})", gen_type(ctx.types[func.ret_type].clone()), gen_function_label(func.label), args);
    code += "{\n";
    let has_vars = !func.variables.is_empty();
    for var in func.variables {
        let typ = ctx.types[ctx.var_types[var]].clone();
        code += &format!("{} {};", gen_type(typ), gen_variable_label(var));
    }
    if has_vars {
        code += "\n";
    }
    code += &gen_block(ctx, func.block);
    while code.ends_with("\n") {
        code.pop();
    }
    code = code.replace("\n", "\n  ");
    code += "\n}\n";
    code
}