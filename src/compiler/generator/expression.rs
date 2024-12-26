use crate::compiler::error::{CompilerError, CompilerResult};
use crate::compiler::generator::function::{generate_function_call};
use crate::compiler::generator::GlobalContext;
use crate::compiler::generator::structure::{generate_field_access, generate_method_call, generate_struct, generate_struct_instantiation};
use crate::compiler::parser::expression::{Expression, Operator};

#[derive(Clone, Eq, Hash, PartialEq, Debug)]
pub enum ValueType {
    I32,
    I64,
    F32,
    F64,
    String,
    Boolean,
    Reference(Box<ValueType>),
    Struct(String, Vec<ValueType>),
    Void,
}

impl ValueType {
    pub fn to_c_type(&self, context: &mut GlobalContext) -> CompilerResult<String> {
        Ok(match self {
            ValueType::I32 => "int".to_owned(),
            ValueType::I64 => "long".to_owned(),
            ValueType::F32 => "float".to_owned(),
            ValueType::F64 => "double".to_owned(),
            ValueType::String => "char*".to_owned(),
            ValueType::Boolean => "int".to_owned(),
            ValueType::Reference(inner) => format!("{}*", inner.to_c_type(context)?),
            ValueType::Struct(name, fields) => {
                let decl = context.structs.iter().find(|s| s.name == *name).expect("Struct not found").clone();
                generate_struct(context, &decl, fields)?
            }
            ValueType::Void => "void".to_owned(),
        })
    }
}

fn add_operator(context: &mut GlobalContext, val1: ValueType, val2: ValueType, return_val: ValueType, op: Operator, op_str: &str) {
    let op_str = op_str.to_owned();
    let func = move |str1, str2| { format!("({str1} {op_str} {str2})") };
    context.operators.insert((val1, op, val2), (Box::new(func), return_val));
}

pub fn setup_default_operators(context: &mut GlobalContext) {
    add_operator(context, ValueType::I32, ValueType::I32, ValueType::I32, Operator::Plus, "+");
    add_operator(context, ValueType::I32, ValueType::I32, ValueType::I32, Operator::Mul, "*");
    add_operator(context, ValueType::I32, ValueType::I32, ValueType::Boolean, Operator::Equals, "==");
    add_operator(context, ValueType::I32, ValueType::I32, ValueType::Boolean, Operator::Less, "<");
    add_operator(context, ValueType::I32, ValueType::I32, ValueType::Boolean, Operator::Greater, ">");
    add_operator(context, ValueType::I32, ValueType::I32, ValueType::Boolean, Operator::LessEquals, "<=");
    add_operator(context, ValueType::I32, ValueType::I32, ValueType::Boolean, Operator::GreaterEquals, ">=");
    add_operator(context, ValueType::I32, ValueType::I32, ValueType::I32, Operator::Minus, "-");
    add_operator(context, ValueType::I32, ValueType::I32, ValueType::Boolean, Operator::NotEquals, "!=");
}

// third return type says if the value is physical (you can take a reference of it)
pub fn generate_expression(context: &mut GlobalContext, expression: &Expression) -> CompilerResult<(String, ValueType, bool)> {
    match expression {
        Expression::Integer(val) => {
            Ok((val.to_string(), ValueType::I32, false))
        }
        Expression::Float(val) => {
            Ok((val.to_string(), ValueType::F32, false))
        }
        Expression::String(val) => {
            Ok((format!("\"{}\"", val), ValueType::String, false))
        }
        Expression::Boolean(val) => {
            Ok((if *val { "1".to_owned() } else { "0".to_owned() }, ValueType::Boolean, false))
        }
        Expression::Variable(ident, pos) => {
            if let Some(typ) = context.get_variable_type(ident) {
                Ok((ident.clone(), typ, true))
            } else {
                Err(CompilerError {
                    message: format!("Variable {} not found", ident),
                    position: Some(pos.clone()),
                })
            }
        }
        Expression::StructInitialization(name, args) => {
            let res = generate_struct_instantiation(context, name.clone(), args)?;
            Ok((res.0, res.1, false))
        }
        Expression::Reference(expr, pos) => {
            let (code, typ, is_phys) = generate_expression(context, expr)?;
            if !is_phys {
                return Err(CompilerError {
                    message: "Cannot take reference of non-physical value".to_string(),
                    position: Some(pos.clone()),
                });
            }

            Ok((format!("&{}", code), ValueType::Reference(Box::new(typ.clone())), false))
        }
        Expression::Dereference(expr, pos) => {
            let (code, typ, _) = generate_expression(context, expr)?;
            if let ValueType::Reference(typ) = typ {
                Ok((format!("*{}", code), *typ, true))
            } else {
                Err(CompilerError {
                    message: "Cannot dereference a non-reference type".to_string(),
                    position: Some(pos.clone()),
                })
            }
        }
        Expression::FunctionCall(name, args) => {
            let res = generate_function_call(context, name, args)?;
            Ok((res.0, res.1, false))
        }
        Expression::BinaryOperation(val1, op, val2, pos) => {
            let (val1_code, val1_type, _) = generate_expression(context, val1)?;
            let (val2_code, val2_type, _) = generate_expression(context, val2)?;

            let operator = context.operators.get(&(val1_type.clone(), op.clone(), val2_type.clone()));

            let (func, return_val) = match operator {
                Some(val) => val,
                None => return Err(CompilerError {
                    message: format!("Operator for {:?} {:?} {:?} not found", val1_type, op, val2_type),
                    position: Some(pos.clone()),
                }),
            };

            Ok((func(val1_code, val2_code), return_val.clone(), false))
        }
        Expression::FieldAccess(expr, field, pos) => {
            let (expr_code, expr_type, is_phys) = generate_expression(context, expr)?;
            let res = generate_field_access(context, expr_code, expr_type, field, pos)?;
            Ok((res.0, res.1, is_phys))
        }
        Expression::MethodCall(expr, expr_pos, method, args) => {
            let res = generate_method_call(context, method, expr, expr_pos, args)?;
            Ok((res.0, res.1, false))
        }
    }
}