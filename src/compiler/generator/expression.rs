use crate::compiler::error::{CompilerError, CompilerResult};
use crate::compiler::generator::function::generate_function;
use crate::compiler::generator::GlobalContext;
use crate::compiler::generator::structure::generate_struct;
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
}

pub fn generate_expression(context: &mut GlobalContext, expression: &Expression) -> CompilerResult<(String, ValueType)> {
    match expression {
        Expression::Integer(val) => {
            Ok((val.to_string(), ValueType::I32))
        }
        Expression::Float(val) => {
            Ok((val.to_string(), ValueType::F32))
        }
        Expression::String(val) => {
            Ok((format!("\"{}\"", val), ValueType::String))
        }
        Expression::Boolean(val) => {
            Ok((val.to_string(), ValueType::Boolean))
        }
        Expression::Variable(ident, pos) => {
            if let Some(typ) = context.get_variable_type(ident) {
                Ok((ident.clone(), typ))
            } else {
                Err(CompilerError {
                    message: format!("Variable {} not found", ident),
                    position: Some(pos.clone()),
                })
            }
        }
        Expression::StructInitialization(name, args) => {
            let declaration = context.structs.iter().find(|s| s.name == *name).expect("Struct not found").clone();
            let mut arg_types = Vec::new();
            let mut arg_codes = Vec::new();
            for arg in args {
                let res = generate_expression(context, arg)?;
                arg_types.push(res.1);
                arg_codes.push(res.0);
            }

            let c_name = generate_struct(context, &declaration, &arg_types)?;
            let typ = ValueType::Struct(name.clone(), arg_types);

            let mut code = String::new();
            code.push_str(&format!("({c_name}){{"));
            for arg in &arg_codes {
                code.push_str(arg);
                code.push_str(",");
            }
            if !arg_codes.is_empty() {
            code.pop();
                }
            code.push_str("}");
            Ok((code, typ))
        }
        Expression::Reference(ident, pos) => {
            if let Some(typ) = context.get_variable_type(ident) {
                Ok((format!("&{}", ident), ValueType::Reference(Box::new(typ.clone()))))
            } else {
                Err(CompilerError {
                    message: format!("Variable {} not found", ident),
                    position: Some(pos.clone()),
                })
            }
        }
        Expression::FunctionCall(name, args) => {
            let (signature, block) = context.functions.iter().find(|f| f.0.name == *name).expect("Function not found").clone();
            let mut arg_types = Vec::new();
            let mut arg_codes = Vec::new();
            for arg in args {
                let res = generate_expression(context, arg)?;
                arg_types.push(res.1);
                arg_codes.push(res.0);
            }

            let (func_name, return_val) = generate_function(context, &signature, &block, &arg_types)?;

            let mut code = func_name;
            code.push_str("(");
            for arg in &arg_codes {
                code.push_str(arg);
                code.push_str(",");
            }
            if !arg_codes.is_empty() {
                assert_eq!(code.pop(), Some(','));
            }
            code.push_str(")");
            Ok((code, return_val))
        }
        Expression::BinaryOperation(val1, op, val2, pos) => {
            let (val1_code, val1_type) = generate_expression(context, val1)?;
            let (val2_code, val2_type) = generate_expression(context, val2)?;

            let operator = context.operators.get(&(val1_type.clone(), op.clone(), val2_type.clone()));

            let (func, return_val) = match operator {
                Some(val) => val,
                None => return Err(CompilerError {
                    message: format!("Operator for {:?} {:?} {:?} not found", val1_type, op, val2_type),
                    position: Some(pos.clone()),
                }),
            };

            Ok((func(val1_code, val2_code), return_val.clone()))
        }
    }
}