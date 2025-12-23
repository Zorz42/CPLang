use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::lowerer::transform_function_name;
use crate::compiler::normalizer::ir::{BuiltinFunctionCall, IRExpression, IRPrimitiveType, IRTypeLabel};
use crate::compiler::normalizer::{Normalizer, ValuePhysicality};

pub fn is_builtin(name: &str) -> bool {
    name.starts_with("_builtin")
}

impl Normalizer {
    pub fn is_builtin_function(function_name: &String) -> bool {
        function_name == &transform_function_name("_builtin_alloc".to_string()) || function_name == &transform_function_name("_builtin_index".to_string())
    }

    pub fn get_builtin_call(
        &mut self,
        function_name: String,
        expr_types: Vec<IRTypeLabel>,
        mut function_arguments: Vec<IRExpression>,
        template_types: Vec<IRTypeLabel>,
        call_pos: FilePosition,
    ) -> CompilerResult<(BuiltinFunctionCall, ValuePhysicality, IRTypeLabel)> {
        let alloc_label = transform_function_name("_builtin_alloc".to_string());
        let index_label = transform_function_name("_builtin_index".to_string());

        match function_name {
            label if label == alloc_label => {
                if function_arguments.len() != 1 {
                    return Err(CompilerError {
                        message: format!("Alloc function takes exactly 1 argument, not {}", function_arguments.len()),
                        position: Some(call_pos),
                    });
                }

                if template_types.len() > 1 {
                    return Err(CompilerError {
                        message: format!("Alloc function takes at most 1 template argument, not {}", template_types.len()),
                        position: Some(call_pos),
                    });
                }

                // index should be an integer
                self.type_resolver.hint_is(expr_types[0], IRPrimitiveType::I32)?;

                let typ = self.type_resolver.new_type_label(FilePosition::unknown());
                let ref_typ = self.type_resolver.new_type_label(FilePosition::unknown());
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
                    ValuePhysicality::Temporary,
                    ref_typ,
                ))
            }

            label if label == index_label => {
                if function_arguments.len() != 2 {
                    return Err(CompilerError {
                        message: format!("Index function takes exactly 2 arguments, not {}", function_arguments.len()),
                        position: Some(call_pos),
                    });
                }

                if !template_types.is_empty() {
                    return Err(CompilerError {
                        message: format!("Index function takes no template arguments, got {}", template_types.len()),
                        position: Some(call_pos),
                    });
                }

                let index_expr = function_arguments.pop().unwrap();
                let arr_expr = function_arguments.pop().unwrap();

                let arr_type = self.type_resolver.new_type_label(FilePosition::unknown());

                self.type_resolver.hint_is(expr_types[1], IRPrimitiveType::I32)?;
                self.type_resolver.hint_is_ref(arr_type, expr_types[0])?;

                Ok((
                    BuiltinFunctionCall::Index {
                        arr: Box::new(arr_expr),
                        idx: Box::new(index_expr),
                    },
                    ValuePhysicality::Physical,
                    arr_type,
                ))
            }
            _ => unreachable!(),
        }
    }
}
