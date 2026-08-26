use crate::compiler::normalizer::ir::{
    IR, IRBlock, IRBuiltinFunctionCall, IRConstant, IRExpression, IRFieldLabel, IRInstance, IRInstanceLabel, IRStatement, IRStruct, IRStructLabel, IRType,
    IRTypeLabel, IRVariableLabel,
};
use crate::compiler::parser::ast::PrimitiveType;
use std::collections::HashMap;
/*
Codegen converts IR into raw C code. Could be easily replaced with any other language.
 */

#[allow(clippy::type_complexity)]
struct CodegenContext {
    types: HashMap<IRTypeLabel, IRType>,
    var_types: Vec<IRTypeLabel>,
    structs: Vec<IRStruct>,
    c_structs: HashMap<(IRStructLabel, Vec<IRType>), usize>,
    curr_struct_label: usize,
    struct_declarations: String,
}

const OUTPUT_TEMPLATE: &str = r"
#include<sys/mman.h>
#include<stdint.h>
#include<stdio.h>

#define ARENA_SIZE 1024ull*1024*1024
static uint8_t* arena;

static void bump_init(void) {
    arena=mmap(NULL,ARENA_SIZE,PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_ANONYMOUS,-1,0);
}

static void* bump_malloc(size_t size) {
    size=(size+7)&~7ull;
    void*ptr=arena;
    arena+=size;
    return ptr;
}

{{struct_declarations}}

{{global_variables}}

{{function_signatures}}

{{functions}}

int main(){
    bump_init();
    {{main}}();
    return 0;
}
";

pub fn generate_code(ir: IR) -> String {
    let mut ctx = CodegenContext {
        types: ir.types,
        var_types: ir.variable_types,
        structs: ir.structs,
        c_structs: HashMap::new(),
        curr_struct_label: 0,
        struct_declarations: String::new(),
    };

    let mut global_variables = String::new();
    for var_label in ir.global_variables {
        let var_type = ctx.types[&ctx.var_types[var_label]].clone();
        let var_type = gen_type(&mut ctx, var_type);
        global_variables += &format!("static {} {};\n", var_type, gen_variable_label(var_label));
    }

    let mut function_signatures = String::new();
    for func in &ir.instances {
        function_signatures += &gen_function_signature(&mut ctx, func);
        function_signatures += ";\n";
    }

    let mut functions = String::new();
    for func in ir.instances {
        functions += &gen_function(&mut ctx, func);
    }

    OUTPUT_TEMPLATE
        .replace("{{global_variables}}", &global_variables)
        .replace("{{main}}", &gen_function_label(ir.main_function))
        .replace("{{struct_declarations}}", &ctx.struct_declarations)
        .replace("{{function_signatures}}", &function_signatures)
        .replace("{{functions}}", &functions)
}

fn gen_primitive_type(typ: PrimitiveType) -> String {
    match typ {
        PrimitiveType::I32 | PrimitiveType::Bool => "int",
        PrimitiveType::I64 => "long long",
        PrimitiveType::F32 => "float",
        PrimitiveType::F64 => "double",
        PrimitiveType::String => "char*",
        PrimitiveType::Char => "char",
        PrimitiveType::Void => "void",
    }
        .to_owned()
}

fn gen_struct_name(label: usize) -> String {
    format!("S{label}")
}

fn gen_field_name(label: IRFieldLabel) -> String {
    format!("F{label}")
}

fn gen_struct_declaration(ctx: &mut CodegenContext, fields: Vec<(IRType, IRFieldLabel)>, c_label: usize) -> String {
    let mut code = String::new();

    let c_name = gen_struct_name(c_label);
    code += &format!("typedef struct {c_name} {{\n");

    for (field_type, field_label) in fields {
        code += &format!("    {} {};\n", gen_type(ctx, field_type), gen_field_name(field_label));
    }

    code += &format!("}} {c_name};\n");
    code
}

fn gen_type(ctx: &mut CodegenContext, typ: IRType) -> String {
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

fn gen_builtin_call(ctx: &mut CodegenContext, call: IRBuiltinFunctionCall) -> String {
    fn gen_op(ctx: &mut CodegenContext, arg1: IRExpression, arg2: IRExpression, op: &str) -> String {
        format!("({} {} {})", gen_expression(ctx, arg1), op, gen_expression(ctx, arg2))
    }
    match call {
        IRBuiltinFunctionCall::Alloc { typ, num } => {
            let typ = ctx.types[&typ].clone();
            format!("bump_malloc(sizeof({})*({}))", gen_type(ctx, typ), gen_expression(ctx, *num))
        }
        IRBuiltinFunctionCall::Index { arr, idx } => format!("({})[{}]", gen_expression(ctx, *arr), gen_expression(ctx, *idx)),
        IRBuiltinFunctionCall::IndexStr { string, idx } => format!("({})[{}]", gen_expression(ctx, *string), gen_expression(ctx, *idx)),
        IRBuiltinFunctionCall::Getchar {} => "getchar()".to_string(),
        IRBuiltinFunctionCall::Putchar { arg } => format!("putchar({})", gen_expression(ctx, *arg)),
        IRBuiltinFunctionCall::Cast { arg, to_type } => format!("({})({})", gen_primitive_type(to_type), gen_expression(ctx, *arg)),
        IRBuiltinFunctionCall::Add { arg1, arg2 } => gen_op(ctx, *arg1, *arg2, "+"),
        IRBuiltinFunctionCall::Sub { arg1, arg2 } => gen_op(ctx, *arg1, *arg2, "-"),
        IRBuiltinFunctionCall::Mul { arg1, arg2 } => gen_op(ctx, *arg1, *arg2, "*"),
        IRBuiltinFunctionCall::Div { arg1, arg2 } => gen_op(ctx, *arg1, *arg2, "/"),
        IRBuiltinFunctionCall::Mod { arg1, arg2 } => gen_op(ctx, *arg1, *arg2, "%"),
        IRBuiltinFunctionCall::Eq { arg1, arg2 } => gen_op(ctx, *arg1, *arg2, "=="),
        IRBuiltinFunctionCall::NotEq { arg1, arg2 } => gen_op(ctx, *arg1, *arg2, "!="),
        IRBuiltinFunctionCall::Lesser { arg1, arg2 } => gen_op(ctx, *arg1, *arg2, "<"),
        IRBuiltinFunctionCall::LesserEq { arg1, arg2 } => gen_op(ctx, *arg1, *arg2, "<="),
        IRBuiltinFunctionCall::Greater { arg1, arg2 } => gen_op(ctx, *arg1, *arg2, ">"),
        IRBuiltinFunctionCall::GreaterEq { arg1, arg2 } => gen_op(ctx, *arg1, *arg2, ">="),
        IRBuiltinFunctionCall::And { arg1, arg2 } => gen_op(ctx, *arg1, *arg2, "&&"),
        IRBuiltinFunctionCall::Or { arg1, arg2 } => gen_op(ctx, *arg1, *arg2, "||"),
        IRBuiltinFunctionCall::Not { arg } => format!("(!{})", gen_expression(ctx, *arg)),
    }
}

fn gen_expression(ctx: &mut CodegenContext, expression: IRExpression) -> String {
    match expression {
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
            IRConstant::Char(x) => (x as i32).to_string(),
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
            let field_types = fields_type_labels.into_iter().map(|x| ctx.types[&x].clone()).collect();
            let c_name = gen_type(ctx, IRType::Struct(struct_label, field_types));
            let mut code = format!("({c_name})");
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
        IRExpression::Reference { expression, pos: _ } => format!("(&{})", gen_expression(ctx, *expression)),
        IRExpression::Variable { variable_label } => gen_variable_label(variable_label),
        IRExpression::AutoRef { .. } => unreachable!("IRExpression::AutoRef should not be emitted by normalizer"),
    }
}

fn gen_block(ctx: &mut CodegenContext, block: IRBlock, code_prefix: String) -> String {
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
            IRStatement::Return { return_value } => return_value.map_or_else(
                || "return;".to_string(),
                |return_value| format!("return {};", gen_expression(ctx, return_value)),
            ),
            IRStatement::Assignment { assign_to, value, pos: _ } => {
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

fn gen_function_signature(ctx: &mut CodegenContext, func: &IRInstance) -> String {
    let mut args = String::new();
    for arg in &func.arguments {
        let typ = ctx.types[&ctx.var_types[*arg]].clone();
        args += &format!("{} {},", gen_type(ctx, typ), gen_variable_label(*arg));
    }
    // pop the last "," if it exists
    args.pop();

    format!(
        "static {} {}({})",
        gen_type(ctx, ctx.types[&func.ret_type].clone()),
        gen_function_label(func.label),
        args
    )
}

fn gen_function(ctx: &mut CodegenContext, func: IRInstance) -> String {
    let mut code = gen_function_signature(ctx, &func);

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
