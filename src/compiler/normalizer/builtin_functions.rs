use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::normalizer::ir::{IRBuiltinFunctionCall, IRExpression, IRType, IRTypeLabel};
use crate::compiler::normalizer::{Normalizer, ValuePhysicality};
use crate::compiler::parser::ast::PrimitiveType;

pub fn is_builtin_identifier(name: &str) -> bool {
    name.starts_with("_builtin")
}

const ALLOC_LABEL: &str = "_builtin_alloc";
const INDEX_LABEL: &str = "_builtin_index";
const INDEX_STR_LABEL: &str = "_builtin_index_str";
const GETCHAR_LABEL: &str = "_builtin_getchar";
const PUTCHAR_LABEL: &str = "_builtin_putchar";
const CAST_LABEL: &str = "_builtin_cast";
const ADD_LABEL: &str = "_builtin_add";
const SUB_LABEL: &str = "_builtin_sub";
const MUL_LABEL: &str = "_builtin_mul";
const DIV_LABEL: &str = "_builtin_div";
const MOD_LABEL: &str = "_builtin_mod";
const EQ_LABEL: &str = "_builtin_eq";
const NOTEQ_LABEL: &str = "_builtin_noteq";
const LESSER_LABEL: &str = "_builtin_lesser";
const GREATER_LABEL: &str = "_builtin_greater";
const LESSEREQ_LABEL: &str = "_builtin_lessereq";
const GREATEREQ_LABEL: &str = "_builtin_greatereq";
const AND_LABEL: &str = "_builtin_and";
const OR_LABEL: &str = "_builtin_or";
const NOT_LABEL: &str = "_builtin_not";

macro_rules! define_builtin_ops {
    (
        $self:ident, $fn_name:expr, $args:expr, $expr_types:expr, $template_types:expr, $call_pos:expr;
        $(
            $label:expr => $variant:ident ($returns_bool:expr, [$($allowed_type:path),* $(,)?])
        ),* $(,)?
    ) => {
        match $fn_name {
            $(
                name if name == $label => {
                    let arg2 = $args.pop().unwrap();
                    let arg1 = $args.pop().unwrap();

                    let typ = $self.type_resolver.fetch_final_ir_type($template_types[0])
                        .ok_or_else(|| CompilerError {
                            message: "Template type should be known".to_string(),
                            position: Some($call_pos)
                        })?;

                    match typ {
                        IRType::Primitive($($allowed_type)|*) => {}
                        _ => return Err(CompilerError {
                            message: format!("Builtin operator does not support {typ:?}"),
                            position: Some($call_pos),
                        }),
                    }

                    $self.type_resolver.hint_equal($expr_types[0], $template_types[0])?;
                    $self.type_resolver.hint_equal($expr_types[1], $template_types[0])?;

                    let ret_type = if $returns_bool {
                        let t = $self.type_resolver.new_type_label(FilePosition::unknown());
                        $self.type_resolver.hint_is(t, PrimitiveType::Bool)?;
                        t
                    } else {
                        $template_types[0]
                    };

                    Ok((IRBuiltinFunctionCall::$variant { arg1: Box::new(arg1), arg2: Box::new(arg2) }, ret_type))
                }
            )*
            _ => unreachable!(),
        }
    };
}

impl Normalizer {
    pub fn is_builtin_function(function_name: &str) -> bool {
        [
            ALLOC_LABEL,
            INDEX_LABEL,
            INDEX_STR_LABEL,
            GETCHAR_LABEL,
            PUTCHAR_LABEL,
            CAST_LABEL,
            ADD_LABEL,
            SUB_LABEL,
            MUL_LABEL,
            DIV_LABEL,
            MOD_LABEL,
            EQ_LABEL,
            NOTEQ_LABEL,
            LESSER_LABEL,
            GREATER_LABEL,
            LESSEREQ_LABEL,
            GREATEREQ_LABEL,
            AND_LABEL,
            OR_LABEL,
            NOT_LABEL,
        ]
        .contains(&function_name)
    }

    pub fn get_builtin_call(
        &mut self,
        function_name: &str,
        expr_types: Vec<IRTypeLabel>,
        mut function_arguments: Vec<IRExpression>,
        template_types: Vec<IRTypeLabel>,
        call_pos: FilePosition,
    ) -> CompilerResult<(IRBuiltinFunctionCall, IRTypeLabel)> {
        let num_arguments = match function_name {
            GETCHAR_LABEL => 0,
            ALLOC_LABEL | PUTCHAR_LABEL | CAST_LABEL | NOT_LABEL => 1,
            INDEX_LABEL | INDEX_STR_LABEL | ADD_LABEL | SUB_LABEL | MUL_LABEL | DIV_LABEL | MOD_LABEL | LESSER_LABEL | GREATER_LABEL | LESSEREQ_LABEL
            | GREATEREQ_LABEL | EQ_LABEL | NOTEQ_LABEL | AND_LABEL | OR_LABEL => 2,
            _ => unreachable!(),
        };

        if function_arguments.len() != num_arguments {
            return Err(CompilerError {
                message: format!("{function_name} takes {num_arguments} arguments, not {}", function_arguments.len()),
                position: Some(call_pos),
            });
        }

        let template_arguments_limit = match function_name {
            ALLOC_LABEL => (0, 1),
            CAST_LABEL | ADD_LABEL | SUB_LABEL | MUL_LABEL | DIV_LABEL | MOD_LABEL | LESSER_LABEL | GREATER_LABEL | LESSEREQ_LABEL | GREATEREQ_LABEL
            | EQ_LABEL | NOTEQ_LABEL | AND_LABEL | OR_LABEL => (1, 1),
            INDEX_LABEL | INDEX_STR_LABEL | GETCHAR_LABEL | PUTCHAR_LABEL | NOT_LABEL => (0, 0),
            _ => unreachable!(),
        };

        if template_types.len() < template_arguments_limit.0 || template_arguments_limit.1 < template_types.len() {
            return Err(CompilerError {
                message: format!(
                    "{function_name} function takes at between {} and {} template arguments, not {}",
                    template_arguments_limit.0,
                    template_arguments_limit.1,
                    template_types.len()
                ),
                position: Some(call_pos),
            });
        }

        match function_name {
            ALLOC_LABEL => {
                // size should be an integer
                self.type_resolver.hint_is(expr_types[0], PrimitiveType::I32)?;

                let typ = self.type_resolver.new_type_label(call_pos);
                let ref_typ = self.type_resolver.new_type_label(call_pos);
                self.type_resolver.hint_is_ref(typ, ref_typ)?;

                if let Some(template_typ) = template_types.first() {
                    self.type_resolver.hint_equal(typ, *template_typ)?;
                }
                self.relevant_types.push(typ);

                Ok((
                    IRBuiltinFunctionCall::Alloc {
                        typ,
                        num: Box::new(function_arguments.pop().unwrap()),
                    },
                    ref_typ,
                ))
            }

            INDEX_LABEL => {
                let index_expr = function_arguments.pop().unwrap();
                let arr_expr = function_arguments.pop().unwrap();

                let arr_type = self.type_resolver.new_type_label(FilePosition::unknown());

                // index should be an integer
                self.type_resolver.hint_is(expr_types[1], PrimitiveType::I32)?;
                self.type_resolver.hint_is_ref(arr_type, expr_types[0])?;

                Ok((
                    IRBuiltinFunctionCall::Index {
                        arr: Box::new(arr_expr),
                        idx: Box::new(index_expr),
                    },
                    arr_type,
                ))
            }

            INDEX_STR_LABEL => {
                let index_expr = function_arguments.pop().unwrap();
                let str_expr = function_arguments.pop().unwrap();

                self.type_resolver.hint_is(expr_types[1], PrimitiveType::I32)?;
                self.type_resolver.hint_is(expr_types[0], PrimitiveType::String)?;

                let char_type = self.type_resolver.new_type_label(call_pos);
                self.type_resolver.hint_is(char_type, PrimitiveType::Char)?;

                Ok((
                    IRBuiltinFunctionCall::IndexStr {
                        string: Box::new(str_expr),
                        idx: Box::new(index_expr),
                    },
                    char_type,
                ))
            }

            GETCHAR_LABEL => {
                let char_type = self.type_resolver.new_type_label(FilePosition::unknown());
                self.type_resolver.hint_is(char_type, PrimitiveType::Char)?;
                Ok((IRBuiltinFunctionCall::Getchar {}, char_type))
            }

            PUTCHAR_LABEL => {
                let char_expr = function_arguments.pop().unwrap();

                self.type_resolver.hint_is(expr_types[0], PrimitiveType::Char)?;

                let ret_type = self.type_resolver.new_type_label(FilePosition::unknown());
                self.type_resolver.hint_is(ret_type, PrimitiveType::Void)?;
                Ok((IRBuiltinFunctionCall::Putchar { arg: Box::new(char_expr) }, ret_type))
            }

            NOT_LABEL => {
                let res_type = self.type_resolver.new_type_label(FilePosition::unknown());
                self.type_resolver.hint_is(res_type, PrimitiveType::Bool)?;

                let expr = function_arguments.pop().unwrap();
                self.type_resolver.hint_is(expr_types[0], PrimitiveType::Bool)?;

                Ok((IRBuiltinFunctionCall::Not { arg: Box::new(expr) }, res_type))
            }

            CAST_LABEL => {
                let expr = function_arguments.pop().unwrap();
                let Some(expr_type) = self.type_resolver.fetch_final_ir_type(expr_types[0]) else {
                    return Err(CompilerError {
                        message: "This type must be known at this point".to_owned(),
                        position: Some(call_pos),
                    });
                };

                let cast_to = template_types[0];
                let Some(cast_to) = self.type_resolver.fetch_final_ir_type(cast_to) else {
                    return Err(CompilerError {
                        message: "This type must be known at this point".to_owned(),
                        position: Some(call_pos),
                    });
                };

                match (&expr_type, &cast_to) {
                    // cast i32 and i64 to *
                    (IRType::Primitive(PrimitiveType::I32 | PrimitiveType::I64), IRType::Primitive(
                        PrimitiveType::I32
                        | PrimitiveType::I64
                        | PrimitiveType::F32
                        | PrimitiveType::F64
                        | PrimitiveType::Bool
                        | PrimitiveType::Char))

                    // cast f32 and f64 to *
                    | (IRType::Primitive(PrimitiveType::F32 | PrimitiveType::F64), IRType::Primitive(
                        PrimitiveType::I32
                        | PrimitiveType::I64
                        | PrimitiveType::F32
                        | PrimitiveType::F64))

                    // cast bool to *
                    | (IRType::Primitive(PrimitiveType::Bool), IRType::Primitive(
                        PrimitiveType::Bool
                        | PrimitiveType::I32
                        | PrimitiveType::I64))

                    // cast char to *
                    | (IRType::Primitive(PrimitiveType::Char), IRType::Primitive(
                        PrimitiveType::Char
                        | PrimitiveType::I32
                        | PrimitiveType::I64))

                    => {} // OK

                    _ => return Err(CompilerError {
                        message: format!("Cannot cast {expr_type:?} to {cast_to:?}"),
                        position: Some(call_pos),
                    }),
                }

                let IRType::Primitive(to_type) = cast_to else { unreachable!() };

                let res_type = self.type_resolver.new_type_label(FilePosition::unknown());
                self.type_resolver.hint_is(res_type, to_type.clone())?;

                Ok((IRBuiltinFunctionCall::Cast { arg: Box::new(expr), to_type }, res_type))
            }

            _ => define_builtin_ops!(
                self, function_name, function_arguments, expr_types, template_types, call_pos;
                ADD_LABEL       => Add(false, [PrimitiveType::Char, PrimitiveType::I32, PrimitiveType::I64, PrimitiveType::F32, PrimitiveType::F64]),
                SUB_LABEL       => Sub(false, [PrimitiveType::Char, PrimitiveType::I32, PrimitiveType::I64, PrimitiveType::F32, PrimitiveType::F64]),
                MUL_LABEL       => Mul(false, [PrimitiveType::I32, PrimitiveType::I64, PrimitiveType::F32, PrimitiveType::F64]),
                DIV_LABEL       => Div(false, [PrimitiveType::I32, PrimitiveType::I64, PrimitiveType::F32, PrimitiveType::F64]),
                MOD_LABEL       => Mod(false, [PrimitiveType::I32, PrimitiveType::I64]),
                GREATER_LABEL   => Greater(true, [PrimitiveType::I32, PrimitiveType::I64, PrimitiveType::F32, PrimitiveType::F64, PrimitiveType::Char]),
                LESSER_LABEL    => Lesser(true, [PrimitiveType::I32, PrimitiveType::I64, PrimitiveType::F32, PrimitiveType::F64, PrimitiveType::Char]),
                GREATEREQ_LABEL => GreaterEq(true, [PrimitiveType::I32, PrimitiveType::I64, PrimitiveType::F32, PrimitiveType::F64, PrimitiveType::Char]),
                LESSEREQ_LABEL  => LesserEq(true, [PrimitiveType::I32, PrimitiveType::I64, PrimitiveType::F32, PrimitiveType::F64, PrimitiveType::Char]),
                EQ_LABEL        => Eq(true, [PrimitiveType::I32, PrimitiveType::I64, PrimitiveType::F32, PrimitiveType::F64, PrimitiveType::Char]),
                NOTEQ_LABEL     => NotEq(true, [PrimitiveType::I32, PrimitiveType::I64, PrimitiveType::F32, PrimitiveType::F64, PrimitiveType::Char]),
                AND_LABEL       => And(true, [PrimitiveType::Bool]),
                OR_LABEL        => Or(true, [PrimitiveType::Bool]),
            ),
        }
    }
}

impl IRBuiltinFunctionCall {
    pub const fn get_value_physicality(&self) -> ValuePhysicality {
        match self {
            Self::Index { .. } | Self::IndexStr { .. } => ValuePhysicality::Physical,
            Self::Alloc { .. }
            | Self::Getchar { .. }
            | Self::Putchar { .. }
            | Self::Cast { .. }
            | Self::And { .. }
            | Self::Or { .. }
            | Self::Add { .. }
            | Self::Sub { .. }
            | Self::Mul { .. }
            | Self::Div { .. }
            | Self::Mod { .. }
            | Self::Eq { .. }
            | Self::NotEq { .. }
            | Self::Lesser { .. }
            | Self::Greater { .. }
            | Self::LesserEq { .. }
            | Self::GreaterEq { .. }
            | Self::Not { .. } => ValuePhysicality::Temporary,
        }
    }
}
