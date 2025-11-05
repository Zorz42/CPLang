use std::collections::HashMap;
use crate::compiler::normalizer::ir::{IRBlock, IRConstant, IRExpression, IRFunction, IRFunctionLabel, IROperator, IRPrimitiveType, IRStatement, IRStruct, IRStructLabel, IRType, IRTypeLabel, IRVariableLabel, IR};

/*
Generator converts IR into raw C code. Could be easily replaced with any other language.

 */

struct GeneratorContext {
    types: Vec<IRType>,
    var_types: Vec<IRTypeLabel>,
    operators: HashMap<(IRType, IROperator, IRType), Box<dyn Fn(String, String) -> String>>,
    structs: Vec<IRStruct>,
    c_structs: HashMap<(IRStructLabel, Vec<IRType>), usize>,
    curr_struct_label: usize,
}

fn init_default_operators() -> HashMap<(IRType, IROperator, IRType), Box<dyn Fn(String, String) -> String>> {
    let mut res: HashMap<(IRType, IROperator, IRType), Box<dyn Fn(String, String) -> String>> = HashMap::new();

    let c_plus = |a, b| { format!("{a} + {b}") };
    let c_minus = |a, b| { format!("{a} - {b}") };
    let c_mul = |a, b| { format!("{a} * {b}") };
    let c_div = |a, b| { format!("{a} / {b}") };
    let c_equal = |a, b| { format!("{a} == {b}") };
    let c_not_equal = |a, b| { format!("{a} != {b}") };
    let c_greater = |a, b| { format!("{a} > {b}") };
    let c_greater_or_eq = |a, b| { format!("{a} >= {b}") };
    let c_lesser = |a, b| { format!("{a} < {b}") };
    let c_lesser_or_eq = |a, b| { format!("{a} <= {b}") };
    res.insert((IRType::Primitive(IRPrimitiveType::I32), IROperator::Plus, IRType::Primitive(IRPrimitiveType::I32)), Box::new(c_plus));
    res.insert((IRType::Primitive(IRPrimitiveType::I32), IROperator::Minus, IRType::Primitive(IRPrimitiveType::I32)), Box::new(c_minus));
    res.insert((IRType::Primitive(IRPrimitiveType::I32), IROperator::Mul, IRType::Primitive(IRPrimitiveType::I32)), Box::new(c_mul));
    res.insert((IRType::Primitive(IRPrimitiveType::I32), IROperator::Div, IRType::Primitive(IRPrimitiveType::I32)), Box::new(c_div));
    res.insert((IRType::Primitive(IRPrimitiveType::I32), IROperator::Equals, IRType::Primitive(IRPrimitiveType::I32)), Box::new(c_equal));
    res.insert((IRType::Primitive(IRPrimitiveType::I32), IROperator::NotEquals, IRType::Primitive(IRPrimitiveType::I32)), Box::new(c_not_equal));
    res.insert((IRType::Primitive(IRPrimitiveType::I32), IROperator::Greater, IRType::Primitive(IRPrimitiveType::I32)), Box::new(c_greater));
    res.insert((IRType::Primitive(IRPrimitiveType::I32), IROperator::GreaterOrEq, IRType::Primitive(IRPrimitiveType::I32)), Box::new(c_greater_or_eq));
    res.insert((IRType::Primitive(IRPrimitiveType::I32), IROperator::Lesser, IRType::Primitive(IRPrimitiveType::I32)), Box::new(c_lesser));
    res.insert((IRType::Primitive(IRPrimitiveType::I32), IROperator::LesserOrEq, IRType::Primitive(IRPrimitiveType::I32)), Box::new(c_lesser_or_eq));

    res
}

pub fn generate_code(ir: IR) -> String {
    let mut code = "#include<stdio.h>\n#include<stdlib.h>\n\n".to_owned();

    let mut ctx = GeneratorContext {
        types: ir.types,
        var_types: ir.variable_types,
        operators: init_default_operators(),
        structs: ir.structs,
        c_structs: HashMap::new(),
        curr_struct_label: 0,
    };

    for func in ir.functions {
        code += &gen_function(&mut ctx, func);
    }

    let main_code = r#"
int main(){
    $main$();
    return 0;
}
    "#;
    code += &main_code.replace("$main$", &gen_function_label(ir.main_function));

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

fn gen_struct_name(label: usize) -> String {
    format!("S{label}")
}

fn gen_type(ctx: &mut GeneratorContext, typ: IRType) -> String {
    match typ {
        IRType::Primitive(typ) => gen_primitive_type(typ),
        IRType::Reference(typ) => format!("{}*", gen_type(ctx, *typ)),
        IRType::Tuple(_) => {
            todo!()
        }
        IRType::Enum(_) => {
            todo!()
        }
        IRType::Struct(label, args) => {
            let gen_label =
                if let Some(gen_label) = ctx.c_structs.get(&(label, args.clone())) {
                    *gen_label
                } else {
                    let gen_label = ctx.curr_struct_label;
                    ctx.curr_struct_label += 1;
                    ctx.c_structs.insert((label, args.clone()), gen_label);
                    gen_label
                };
            gen_struct_name(gen_label)
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
        IRType::Struct(_, _) => todo!(),
    }
}

fn gen_expression(ctx: &mut GeneratorContext, expression: IRExpression) -> String {
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
                IRConstant::String(x) => {
                    let mut escaped = String::new();
                    for c in x.chars() {
                        match c {
                            '\n' => escaped.push_str("\\n"),
                            '\r' => escaped.push_str("\\r"),
                            '\t' => escaped.push_str("\\t"),
                            '\"' => escaped.push_str("\\\""),
                            '\\' => escaped.push_str("\\\\"),
                            _ => escaped.push(c),
                        }
                    }
                    format!("\"{escaped}\"")
                }
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
        IRExpression::Dereference(expr) => format!("(*{})", gen_expression(ctx, *expr)),
        IRExpression::StructInitialization(label, args) => {
            todo!()
        }
        IRExpression::Reference(expr) => format!("(&{})", gen_expression(ctx, *expr)),
        IRExpression::Variable(var) => {
            gen_variable_label(var)
        }
    }
}

fn gen_block(ctx: &mut GeneratorContext, block: IRBlock, code_prefix: String) -> String {
    let mut code = String::new();
    code += "{\n";
    code += &code_prefix;

    for statement in block.statements {
        let s_code = match statement {
            IRStatement::Block(block) => {
                gen_block(ctx, block, String::new())
            }
            IRStatement::If(cond, block, else_block) => {
                let mut code = format!("if({}){}", gen_expression(ctx, cond), gen_block(ctx, block, String::new()));
                if let Some(else_block) = else_block {
                    code += &format!("else {}", gen_block(ctx, else_block, String::new()));
                }
                code
            }
            IRStatement::While(cond, block) => {
                format!("while({}){}", gen_expression(ctx, cond), gen_block(ctx, block, String::new()))
            }
            IRStatement::Expression(expr) => format!("{};", gen_expression(ctx, expr)),
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
    while code.ends_with("\n") {
        code.pop();
    }
    code = code.replace("\n", "\n  ");
    code += "\n}\n";

    code
}

fn gen_function(ctx: &mut GeneratorContext, func: IRFunction) -> String {
    let mut args = String::new();
    for arg in func.arguments {
        let typ = ctx.types[ctx.var_types[arg]].clone();
        args += &format!("{} {},", gen_type(ctx, typ), gen_variable_label(arg));
    }
    // pop the last "," if it exists
    args.pop();

    let mut code = format!("{} {}({})", gen_type(ctx, ctx.types[func.ret_type].clone()), gen_function_label(func.label), args);

    let mut vars_code = String::new();
    for var in func.variables {
        let typ = ctx.types[ctx.var_types[var]].clone();
        vars_code += &format!("{} {};", gen_type(ctx, typ), gen_variable_label(var));
    }
    if !vars_code.is_empty() {
        vars_code += "\n";
    }
    code += &gen_block(ctx, func.block, vars_code);
    code
}