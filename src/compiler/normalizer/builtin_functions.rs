use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::normalizer::ir::{BuiltinFunctionCall, IRExpression, IRType, IRTypeLabel};
use crate::compiler::normalizer::{Normalizer, ValuePhysicality};
use crate::compiler::parser::ast::PrimitiveType;

pub fn is_builtin_identifier(name: &str) -> bool {
    name.starts_with("_builtin")
}

const ALLOC_LABEL: &str = "_builtin_alloc";
const INDEX_LABEL: &str = "_builtin_index";
const GETCHAR_LABEL: &str = "_builtin_getchar";
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

                    Ok((BuiltinFunctionCall::$variant { arg1: Box::new(arg1), arg2: Box::new(arg2) }, ret_type))
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
            GETCHAR_LABEL,
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
    ) -> CompilerResult<(BuiltinFunctionCall, IRTypeLabel)> {
        let num_arguments = match function_name {
            label if label == ALLOC_LABEL => 1,
            label if label == INDEX_LABEL => 2,
            label if label == GETCHAR_LABEL => 0,
            label if label == ADD_LABEL => 2,
            label if label == SUB_LABEL => 2,
            label if label == MUL_LABEL => 2,
            label if label == DIV_LABEL => 2,
            label if label == MOD_LABEL => 2,
            label if label == LESSER_LABEL => 2,
            label if label == GREATER_LABEL => 2,
            label if label == LESSEREQ_LABEL => 2,
            label if label == GREATEREQ_LABEL => 2,
            label if label == EQ_LABEL => 2,
            label if label == NOTEQ_LABEL => 2,
            label if label == AND_LABEL => 2,
            label if label == OR_LABEL => 2,
            label if label == NOT_LABEL => 1,
            _ => unreachable!(),
        };

        if function_arguments.len() != num_arguments {
            return Err(CompilerError {
                message: format!("{function_name} takes {num_arguments} arguments, not {}", function_arguments.len()),
                position: Some(call_pos),
            });
        }

        let template_arguments_limit = match function_name {
            label if label == ALLOC_LABEL => (0, 1),
            label if label == INDEX_LABEL => (0, 0),
            label if label == GETCHAR_LABEL => (0, 0),
            label if label == ADD_LABEL => (1, 1),
            label if label == SUB_LABEL => (1, 1),
            label if label == MUL_LABEL => (1, 1),
            label if label == DIV_LABEL => (1, 1),
            label if label == MOD_LABEL => (1, 1),
            label if label == LESSER_LABEL => (1, 1),
            label if label == GREATER_LABEL => (1, 1),
            label if label == LESSEREQ_LABEL => (1, 1),
            label if label == GREATEREQ_LABEL => (1, 1),
            label if label == EQ_LABEL => (1, 1),
            label if label == NOTEQ_LABEL => (1, 1),
            label if label == AND_LABEL => (1, 1),
            label if label == OR_LABEL => (1, 1),
            label if label == NOT_LABEL => (0, 0),
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
                    BuiltinFunctionCall::Alloc {
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
                    BuiltinFunctionCall::Index {
                        arr: Box::new(arr_expr),
                        idx: Box::new(index_expr),
                    },
                    arr_type,
                ))
            }

            GETCHAR_LABEL => {
                let char_type = self.type_resolver.new_type_label(FilePosition::unknown());
                self.type_resolver.hint_is(char_type, PrimitiveType::Char)?;
                Ok((BuiltinFunctionCall::Getchar {}, char_type))
            }

            NOT_LABEL => {
                let res_type = self.type_resolver.new_type_label(FilePosition::unknown());
                self.type_resolver.hint_is(res_type, PrimitiveType::Bool)?;

                let expr = function_arguments.pop().unwrap();
                self.type_resolver.hint_is(expr_types[0], PrimitiveType::Bool)?;

                Ok((BuiltinFunctionCall::Not { arg: Box::new(expr) }, res_type))
            }

            _ => define_builtin_ops!(
                self, function_name, function_arguments, expr_types, template_types, call_pos;
                ADD_LABEL       => Add(false, [PrimitiveType::I32, PrimitiveType::I64, PrimitiveType::F32, PrimitiveType::F64]),
                SUB_LABEL       => Sub(false, [PrimitiveType::I32, PrimitiveType::I64, PrimitiveType::F32, PrimitiveType::F64]),
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

impl BuiltinFunctionCall {
    pub const fn get_value_physicality(&self) -> ValuePhysicality {
        match self {
            Self::Index { .. } => ValuePhysicality::Physical,
            Self::Alloc { .. }
            | Self::Getchar { .. }
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
