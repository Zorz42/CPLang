use crate::compiler::generator::default_operators::init_default_operators;
use crate::compiler::normalizer::ir::{BuiltinFunctionCall, IRBlock, IRConstant, IRExpression, IRFieldLabel, IRInstance, IRInstanceLabel, IROperator, IRPrimitiveType, IRStatement, IRStruct, IRStructLabel, IRType, IRTypeLabel, IRVariableLabel, IR};
use std::collections::HashMap;

mod default_operators;

/*
Generator converts IR into raw C code. Could be easily replaced with any other language.
 */

#[allow(clippy::type_complexity)]
struct GeneratorContext {
    types: HashMap<IRTypeLabel, IRType>,
    var_types: Vec<IRTypeLabel>,
    operators: HashMap<(IRType, IROperator, IRType), Box<dyn Fn(String, String) -> String>>,
    structs: Vec<IRStruct>,
    c_structs: HashMap<(IRStructLabel, Vec<IRType>), usize>,
    curr_struct_label: usize,
    struct_declarations: String,
    autorefs: Vec<i32>,
}

pub fn generate_code(ir: IR) -> String {
    let mut code = String::new();
    let imports = "#include<stdio.h>\n#include<stdlib.h>\n\n".to_owned();

    let mut ctx = GeneratorContext {
        types: ir.types,
        var_types: ir.variable_types,
        operators: init_default_operators(),
        structs: ir.structs,
        c_structs: HashMap::new(),
        curr_struct_label: 0,
        struct_declarations: String::new(),
        autorefs: ir.autorefs,
    };

    for func in ir.instances {
        code += &gen_function(&mut ctx, func);
    }

    let main_code = "
int main(){
    $main$();
    return 0;
}
    ";
    let main_code = main_code.replace("$main$", &gen_function_label(ir.main_function));

    format!("{}\n{}\n{}\n{}", imports, ctx.struct_declarations, code, main_code)
}

fn gen_primitive_type(typ: IRPrimitiveType) -> String {
    match typ {
        IRPrimitiveType::I32 | IRPrimitiveType::Bool => "int",
        IRPrimitiveType::I64 => "long",
        IRPrimitiveType::F32 => "float",
        IRPrimitiveType::F64 => "double",
        IRPrimitiveType::String => "char*",
        IRPrimitiveType::Void => "void",
    }
        .to_owned()
}

fn gen_struct_name(label: usize) -> String {
    format!("S{label}")
}

fn gen_field_name(label: IRFieldLabel) -> String {
    format!("F{label}")
}

fn gen_struct_declaration(ctx: &mut GeneratorContext, fields: Vec<(IRType, IRFieldLabel)>, c_label: usize) -> String {
    let mut code = String::new();

    let c_name = gen_struct_name(c_label);
    code += &format!("typedef struct {c_name} {{\n");

    for (field_type, field_label) in fields {
        code += &format!("    {} {};\n", gen_type(ctx, field_type), gen_field_name(field_label));
    }

    code += &format!("}} {c_name};\n");
    code
}

fn gen_type(ctx: &mut GeneratorContext, typ: IRType) -> String {
    match typ {
        IRType::Primitive(typ) => gen_primitive_type(typ),
        IRType::Reference(typ) => format!("{}*", gen_type(ctx, *typ)),
        IRType::Struct(label, args) => {
            let gen_label = if let Some(gen_label) = ctx.c_structs.get(&(label, args.clone())) {
                *gen_label
            } else {
                let gen_label = ctx.curr_struct_label;
                ctx.curr_struct_label += 1;
                ctx.c_structs.insert((label, args.clone()), gen_label);

                let field_labels = ctx.structs[label].fields.clone();
                let fields = args.into_iter().zip(field_labels).collect();

                let code = gen_struct_declaration(ctx, fields, gen_label);
                ctx.struct_declarations += &code;

                gen_label
            };
            gen_struct_name(gen_label)
        }
    }
}

fn gen_function_label(func: IRInstanceLabel) -> String {
    format!("func{func}")
}

fn gen_variable_label(func: IRVariableLabel) -> String {
    format!("var{func}")
}

fn type_to_printf_format(typ: &IRType) -> &'static str {
    match typ {
        IRType::Primitive(typ) => match typ {
            IRPrimitiveType::I32 | IRPrimitiveType::I64 => "ld",
            IRPrimitiveType::F32 => "f",
            IRPrimitiveType::F64 => "lf",
            IRPrimitiveType::Bool => "d",
            IRPrimitiveType::String => "s",
            IRPrimitiveType::Void => unreachable!(),
        },
        IRType::Reference(typ) => type_to_printf_format(typ),
        IRType::Struct(_, _) => todo!(),
    }
}

fn gen_builtin_call(ctx: &mut GeneratorContext, call: BuiltinFunctionCall) -> String {
    match call {
        BuiltinFunctionCall::Alloc { typ, num } => {
            let typ = ctx.types[&typ].clone();
            format!("malloc(sizeof({})*({}))", gen_type(ctx, typ), gen_expression(ctx, *num))
        }
        BuiltinFunctionCall::Index { arr, idx } => {
            format!("({})[{}]", gen_expression(ctx, *arr), gen_expression(ctx, *idx))
        }
    }
}

fn gen_expression(ctx: &mut GeneratorContext, expression: IRExpression) -> String {
    match expression {
        IRExpression::BinaryOperation {
            operator,
            expression1,
            type1_label,
            expression2,
            type2_label,
        } => {
            let code1 = gen_expression(ctx, *expression1);
            let code2 = gen_expression(ctx, *expression2);
            let typ1 = ctx.types[&type1_label].clone();
            let typ2 = ctx.types[&type2_label].clone();
            format!("({})", ctx.operators[&(typ1, operator, typ2)](code1, code2))
        }
        IRExpression::Constant { constant } => match constant {
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
            IRConstant::Bool(x) => (if x { "1" } else { "0" }).to_string(),
        },
        IRExpression::InstanceCall {
            instance_label: function_label,
            instance_arguments: function_arguments,
        } => {
            let mut args_code = String::new();

            for arg in function_arguments {
                args_code += &gen_expression(ctx, arg);
                args_code += ",";
            }
            // pop the last "," if possible
            args_code.pop();

            format!("{}({})", gen_function_label(function_label), args_code)
        }
        IRExpression::BuiltinFunctionCall(call) => gen_builtin_call(ctx, call),
        IRExpression::FieldAccess { expression, field_label } => {
            format!("{}.{}", gen_expression(ctx, *expression), gen_field_name(field_label))
        }
        IRExpression::Dereference { expression } => format!("(*{})", gen_expression(ctx, *expression)),
        IRExpression::StructInitialization {
            struct_label,
            fields_type_labels,
            field_values,
        } => {
            let fields_type_labels = fields_type_labels.into_iter().map(|x| ctx.types[&x].clone()).collect();
            let c_label = ctx.c_structs[&(struct_label, fields_type_labels)];
            let mut code = format!("({})", gen_struct_name(c_label));
            code += "{";
            for arg in field_values {
                code += &gen_expression(ctx, arg);
                code += ",";
            }
            if code.ends_with(',') {
                code.pop();
            }
            code += "}";
            code
        }
        IRExpression::Reference { expression } => format!("(&{})", gen_expression(ctx, *expression)),
        IRExpression::Variable { variable_label } => gen_variable_label(variable_label),
        IRExpression::AutoRef { autoref_label, expression } => {
            let mut prefix = String::new();

            let ref_depth = ctx.autorefs[autoref_label];
            if ref_depth > 0 {
                for _ in 0..ref_depth {
                    prefix.push('&');
                }
            } else {
                for _ in 0..-ref_depth {
                    prefix.push('*');
                }
            }

            format!("({}{})", prefix, gen_expression(ctx, *expression))
        }
    }
}

fn gen_block(ctx: &mut GeneratorContext, block: IRBlock, code_prefix: String) -> String {
    let mut code = String::new();
    code += "{\n";
    code += &code_prefix;

    for statement in block.statements {
        let s_code = match statement {
            IRStatement::Block { block } => gen_block(ctx, block, String::new()),
            IRStatement::If { condition, block, else_block } => {
                let mut code = format!("if({}){}", gen_expression(ctx, condition), gen_block(ctx, block, String::new()));
                if let Some(else_block) = else_block {
                    code += &format!("else {}", gen_block(ctx, else_block, String::new()));
                }
                code
            }
            IRStatement::While { condition, block } => {
                format!("while({}){}", gen_expression(ctx, condition), gen_block(ctx, block, String::new()))
            }
            IRStatement::Expression { expr } => format!("{};", gen_expression(ctx, expr)),
            IRStatement::Print { expr, type_label } => {
                let typ = ctx.types[&type_label].clone();
                if typ == IRType::Primitive(IRPrimitiveType::Void) {
                    String::new()
                } else {
                    format!("printf(\"%{}\",{});", type_to_printf_format(&typ), gen_expression(ctx, expr))
                }
            }
            IRStatement::Return { return_value } => return_value.map_or_else(
                || "return;".to_string(),
                |return_value| format!("return {};", gen_expression(ctx, return_value)),
            ),
            IRStatement::Assignment { assign_to, value } => {
                format!("{} = {};", gen_expression(ctx, assign_to), gen_expression(ctx, value))
            }
        };
        code += &s_code;
        code += "\n";
    }
    while code.ends_with('\n') {
        code.pop();
    }
    code = code.replace('\n', "\n  ");
    code += "\n}\n";

    code
}

fn gen_function(ctx: &mut GeneratorContext, func: IRInstance) -> String {
    let mut args = String::new();
    for arg in func.arguments {
        let typ = ctx.types[&ctx.var_types[arg]].clone();
        args += &format!("{} {},", gen_type(ctx, typ), gen_variable_label(arg));
    }
    // pop the last "," if it exists
    args.pop();

    let mut code = format!(
        "{} {}({})",
        gen_type(ctx, ctx.types[&func.ret_type].clone()),
        gen_function_label(func.label),
        args
    );

    let mut vars_code = String::new();
    for var in func.variables {
        let typ = ctx.types[&ctx.var_types[var]].clone();
        vars_code += &format!("{} {};", gen_type(ctx, typ), gen_variable_label(var));
    }
    if !vars_code.is_empty() {
        vars_code += "\n";
    }
    code += &gen_block(ctx, func.block, vars_code);
    code
}
