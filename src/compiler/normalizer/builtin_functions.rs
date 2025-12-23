use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::lowerer::transform_function_name;
use crate::compiler::normalizer::ir::{BuiltinFunctionCall, IRExpression, IRPrimitiveType, IRType, IRTypeLabel};
use crate::compiler::normalizer::{Normalizer, ValuePhysicality};

pub fn is_builtin_identifier(name: &str) -> bool {
    name.starts_with("_builtin")
}

impl Normalizer {
    pub fn is_builtin_function(function_name: &String) -> bool {
        function_name == &transform_function_name("_builtin_alloc".to_string()) ||
            function_name == &transform_function_name("_builtin_index".to_string()) ||
            function_name == &transform_function_name("_builtin_add".to_string()) ||
            function_name == &transform_function_name("_builtin_sub".to_string()) ||
            function_name == &transform_function_name("_builtin_mul".to_string()) ||
            function_name == &transform_function_name("_builtin_div".to_string()) ||
            function_name == &transform_function_name("_builtin_eq".to_string()) ||
            function_name == &transform_function_name("_builtin_noteq".to_string()) ||
            function_name == &transform_function_name("_builtin_lesser".to_string()) ||
            function_name == &transform_function_name("_builtin_greater".to_string()) ||
            function_name == &transform_function_name("_builtin_lessereq".to_string()) ||
            function_name == &transform_function_name("_builtin_greatereq".to_string())
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
        let add_label = transform_function_name("_builtin_add".to_string());
        let sub_label = transform_function_name("_builtin_sub".to_string());
        let mul_label = transform_function_name("_builtin_mul".to_string());
        let div_label = transform_function_name("_builtin_div".to_string());
        let eq_label = transform_function_name("_builtin_eq".to_string());
        let noteq_label = transform_function_name("_builtin_noteq".to_string());
        let lesser_label = transform_function_name("_builtin_lesser".to_string());
        let greater_label = transform_function_name("_builtin_greater".to_string());
        let lessereq_label = transform_function_name("_builtin_lessereq".to_string());
        let greatereq_label = transform_function_name("_builtin_greatereq".to_string());

        let num_arguments = match &function_name {
            label if label == &alloc_label => 1,
            label if label == &index_label => 2,
            label if label == &add_label => 2,
            label if label == &sub_label => 2,
            label if label == &mul_label => 2,
            label if label == &div_label => 2,
            label if label == &lesser_label => 2,
            label if label == &greater_label => 2,
            label if label == &lessereq_label => 2,
            label if label == &greatereq_label => 2,
            label if label == &eq_label => 2,
            label if label == &noteq_label => 2,
            _ => unreachable!(),
        };

        if function_arguments.len() != num_arguments {
            return Err(CompilerError {
                message: format!("{function_name} takes {num_arguments} arguments, not {}", function_arguments.len()),
                position: Some(call_pos),
            });
        }

        let template_arguments_limit = match &function_name {
            label if label == &alloc_label => (0, 1),
            label if label == &index_label => (0, 0),
            label if label == &add_label => (1, 1),
            label if label == &sub_label => (1, 1),
            label if label == &mul_label => (1, 1),
            label if label == &div_label => (1, 1),
            label if label == &lesser_label => (1, 1),
            label if label == &greater_label => (1, 1),
            label if label == &lessereq_label => (1, 1),
            label if label == &greatereq_label => (1, 1),
            label if label == &eq_label => (1, 1),
            label if label == &noteq_label => (1, 1),
            _ => unreachable!(),
        };

        if template_types.len() < template_arguments_limit.0 || template_arguments_limit.1 < template_types.len() {
            return Err(CompilerError {
                message: format!("{function_name} function takes at between {} and {} template arguments, not {}", template_arguments_limit.0, template_arguments_limit.1, template_types.len()),
                position: Some(call_pos),
            });
        }

        match function_name {
            label if label == alloc_label => {
                // size should be an integer
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
                let index_expr = function_arguments.pop().unwrap();
                let arr_expr = function_arguments.pop().unwrap();

                let arr_type = self.type_resolver.new_type_label(FilePosition::unknown());

                // index should be an integer
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

            label if label == add_label => {
                let arg2 = function_arguments.pop().unwrap();
                let arg1 = function_arguments.pop().unwrap();

                let typ = self.type_resolver.fetch_final_ir_type(template_types[0]);
                let Some(typ) = typ else {
                    return Err(CompilerError {
                        message: "Template type should be known by now".to_string(),
                        position: Some(call_pos),
                    });
                };

                match typ {
                    IRType::Primitive(IRPrimitiveType::I32) |
                    IRType::Primitive(IRPrimitiveType::I64) |
                    IRType::Primitive(IRPrimitiveType::F32) |
                    IRType::Primitive(IRPrimitiveType::F64)
                    => {} // ok
                    _ => return Err(CompilerError {
                        message: format!("Builtin operator does not support {typ:?}"),
                        position: Some(call_pos),
                    })
                }

                self.type_resolver.hint_equal(expr_types[0], template_types[0])?;
                self.type_resolver.hint_equal(expr_types[1], template_types[0])?;

                Ok((
                    BuiltinFunctionCall::Add {
                        arg1: Box::new(arg1),
                        arg2: Box::new(arg2),
                    },
                    ValuePhysicality::Temporary,
                    template_types[0],
                ))
            }

            label if label == sub_label => {
                let arg2 = function_arguments.pop().unwrap();
                let arg1 = function_arguments.pop().unwrap();

                let typ = self.type_resolver.fetch_final_ir_type(template_types[0]);
                let Some(typ) = typ else {
                    return Err(CompilerError {
                        message: "Template type should be known by now".to_string(),
                        position: Some(call_pos),
                    });
                };

                match typ {
                    IRType::Primitive(IRPrimitiveType::I32) |
                    IRType::Primitive(IRPrimitiveType::I64) |
                    IRType::Primitive(IRPrimitiveType::F32) |
                    IRType::Primitive(IRPrimitiveType::F64)
                    => {} // ok
                    _ => return Err(CompilerError {
                        message: format!("Builtin operator does not support {typ:?}"),
                        position: Some(call_pos),
                    })
                }

                self.type_resolver.hint_equal(expr_types[0], template_types[0])?;
                self.type_resolver.hint_equal(expr_types[1], template_types[0])?;

                Ok((
                    BuiltinFunctionCall::Sub {
                        arg1: Box::new(arg1),
                        arg2: Box::new(arg2),
                    },
                    ValuePhysicality::Temporary,
                    template_types[0],
                ))
            }

            label if label == mul_label => {
                let arg2 = function_arguments.pop().unwrap();
                let arg1 = function_arguments.pop().unwrap();

                let typ = self.type_resolver.fetch_final_ir_type(template_types[0]);
                let Some(typ) = typ else {
                    return Err(CompilerError {
                        message: "Template type should be known by now".to_string(),
                        position: Some(call_pos),
                    });
                };

                match typ {
                    IRType::Primitive(IRPrimitiveType::I32) |
                    IRType::Primitive(IRPrimitiveType::I64) |
                    IRType::Primitive(IRPrimitiveType::F32) |
                    IRType::Primitive(IRPrimitiveType::F64)
                    => {} // ok
                    _ => return Err(CompilerError {
                        message: format!("Builtin operator does not support {typ:?}"),
                        position: Some(call_pos),
                    })
                }

                self.type_resolver.hint_equal(expr_types[0], template_types[0])?;
                self.type_resolver.hint_equal(expr_types[1], template_types[0])?;

                Ok((
                    BuiltinFunctionCall::Mul {
                        arg1: Box::new(arg1),
                        arg2: Box::new(arg2),
                    },
                    ValuePhysicality::Temporary,
                    template_types[0],
                ))
            }

            label if label == div_label => {
                let arg2 = function_arguments.pop().unwrap();
                let arg1 = function_arguments.pop().unwrap();

                let typ = self.type_resolver.fetch_final_ir_type(template_types[0]);
                let Some(typ) = typ else {
                    return Err(CompilerError {
                        message: "Template type should be known by now".to_string(),
                        position: Some(call_pos),
                    });
                };

                match typ {
                    IRType::Primitive(IRPrimitiveType::I32) |
                    IRType::Primitive(IRPrimitiveType::I64) |
                    IRType::Primitive(IRPrimitiveType::F32) |
                    IRType::Primitive(IRPrimitiveType::F64)
                    => {} // ok
                    _ => return Err(CompilerError {
                        message: format!("Builtin operator does not support {typ:?}"),
                        position: Some(call_pos),
                    })
                }

                self.type_resolver.hint_equal(expr_types[0], template_types[0])?;
                self.type_resolver.hint_equal(expr_types[1], template_types[0])?;

                Ok((
                    BuiltinFunctionCall::Div {
                        arg1: Box::new(arg1),
                        arg2: Box::new(arg2),
                    },
                    ValuePhysicality::Temporary,
                    template_types[0],
                ))
            }

            label if label == greater_label => {
                let arg2 = function_arguments.pop().unwrap();
                let arg1 = function_arguments.pop().unwrap();

                let typ = self.type_resolver.fetch_final_ir_type(template_types[0]);
                let Some(typ) = typ else {
                    return Err(CompilerError {
                        message: "Template type should be known by now".to_string(),
                        position: Some(call_pos),
                    });
                };

                match typ {
                    IRType::Primitive(IRPrimitiveType::I32) |
                    IRType::Primitive(IRPrimitiveType::I64) |
                    IRType::Primitive(IRPrimitiveType::F32) |
                    IRType::Primitive(IRPrimitiveType::F64)
                    => {} // ok
                    _ => return Err(CompilerError {
                        message: format!("Builtin operator does not support {typ:?}"),
                        position: Some(call_pos),
                    })
                }

                self.type_resolver.hint_equal(expr_types[0], template_types[0])?;
                self.type_resolver.hint_equal(expr_types[1], template_types[0])?;

                let ret_type = self.type_resolver.new_type_label(FilePosition::unknown());
                self.type_resolver.hint_is(ret_type, IRPrimitiveType::Bool)?;

                Ok((
                    BuiltinFunctionCall::Greater {
                        arg1: Box::new(arg1),
                        arg2: Box::new(arg2),
                    },
                    ValuePhysicality::Temporary,
                    ret_type,
                ))
            }

            label if label == lesser_label => {
                let arg2 = function_arguments.pop().unwrap();
                let arg1 = function_arguments.pop().unwrap();

                let typ = self.type_resolver.fetch_final_ir_type(template_types[0]);
                let Some(typ) = typ else {
                    return Err(CompilerError {
                        message: "Template type should be known by now".to_string(),
                        position: Some(call_pos),
                    });
                };

                match typ {
                    IRType::Primitive(IRPrimitiveType::I32) |
                    IRType::Primitive(IRPrimitiveType::I64) |
                    IRType::Primitive(IRPrimitiveType::F32) |
                    IRType::Primitive(IRPrimitiveType::F64)
                    => {} // ok
                    _ => return Err(CompilerError {
                        message: format!("Builtin operator does not support {typ:?}"),
                        position: Some(call_pos),
                    })
                }

                self.type_resolver.hint_equal(expr_types[0], template_types[0])?;
                self.type_resolver.hint_equal(expr_types[1], template_types[0])?;

                let ret_type = self.type_resolver.new_type_label(FilePosition::unknown());
                self.type_resolver.hint_is(ret_type, IRPrimitiveType::Bool)?;

                Ok((
                    BuiltinFunctionCall::Lesser {
                        arg1: Box::new(arg1),
                        arg2: Box::new(arg2),
                    },
                    ValuePhysicality::Temporary,
                    ret_type,
                ))
            }

            label if label == greatereq_label => {
                let arg2 = function_arguments.pop().unwrap();
                let arg1 = function_arguments.pop().unwrap();

                let typ = self.type_resolver.fetch_final_ir_type(template_types[0]);
                let Some(typ) = typ else {
                    return Err(CompilerError {
                        message: "Template type should be known by now".to_string(),
                        position: Some(call_pos),
                    });
                };

                match typ {
                    IRType::Primitive(IRPrimitiveType::I32) |
                    IRType::Primitive(IRPrimitiveType::I64) |
                    IRType::Primitive(IRPrimitiveType::F32) |
                    IRType::Primitive(IRPrimitiveType::F64)
                    => {} // ok
                    _ => return Err(CompilerError {
                        message: format!("Builtin operator does not support {typ:?}"),
                        position: Some(call_pos),
                    })
                }

                self.type_resolver.hint_equal(expr_types[0], template_types[0])?;
                self.type_resolver.hint_equal(expr_types[1], template_types[0])?;

                let ret_type = self.type_resolver.new_type_label(FilePosition::unknown());
                self.type_resolver.hint_is(ret_type, IRPrimitiveType::Bool)?;

                Ok((
                    BuiltinFunctionCall::GreaterEq {
                        arg1: Box::new(arg1),
                        arg2: Box::new(arg2),
                    },
                    ValuePhysicality::Temporary,
                    ret_type,
                ))
            }

            label if label == lessereq_label => {
                let arg2 = function_arguments.pop().unwrap();
                let arg1 = function_arguments.pop().unwrap();

                let typ = self.type_resolver.fetch_final_ir_type(template_types[0]);
                let Some(typ) = typ else {
                    return Err(CompilerError {
                        message: "Template type should be known by now".to_string(),
                        position: Some(call_pos),
                    });
                };

                match typ {
                    IRType::Primitive(IRPrimitiveType::I32) |
                    IRType::Primitive(IRPrimitiveType::I64) |
                    IRType::Primitive(IRPrimitiveType::F32) |
                    IRType::Primitive(IRPrimitiveType::F64)
                    => {} // ok
                    _ => return Err(CompilerError {
                        message: format!("Builtin operator does not support {typ:?}"),
                        position: Some(call_pos),
                    })
                }

                self.type_resolver.hint_equal(expr_types[0], template_types[0])?;
                self.type_resolver.hint_equal(expr_types[1], template_types[0])?;

                let ret_type = self.type_resolver.new_type_label(FilePosition::unknown());
                self.type_resolver.hint_is(ret_type, IRPrimitiveType::Bool)?;

                Ok((
                    BuiltinFunctionCall::LesserEq {
                        arg1: Box::new(arg1),
                        arg2: Box::new(arg2),
                    },
                    ValuePhysicality::Temporary,
                    ret_type,
                ))
            }

            label if label == eq_label => {
                let arg2 = function_arguments.pop().unwrap();
                let arg1 = function_arguments.pop().unwrap();

                let typ = self.type_resolver.fetch_final_ir_type(template_types[0]);
                let Some(typ) = typ else {
                    return Err(CompilerError {
                        message: "Template type should be known by now".to_string(),
                        position: Some(call_pos),
                    });
                };

                match typ {
                    IRType::Primitive(IRPrimitiveType::I32) |
                    IRType::Primitive(IRPrimitiveType::I64) |
                    IRType::Primitive(IRPrimitiveType::F32) |
                    IRType::Primitive(IRPrimitiveType::F64)
                    => {} // ok
                    _ => return Err(CompilerError {
                        message: format!("Builtin operator does not support {typ:?}"),
                        position: Some(call_pos),
                    })
                }

                self.type_resolver.hint_equal(expr_types[0], template_types[0])?;
                self.type_resolver.hint_equal(expr_types[1], template_types[0])?;

                let ret_type = self.type_resolver.new_type_label(FilePosition::unknown());
                self.type_resolver.hint_is(ret_type, IRPrimitiveType::Bool)?;

                Ok((
                    BuiltinFunctionCall::Eq {
                        arg1: Box::new(arg1),
                        arg2: Box::new(arg2),
                    },
                    ValuePhysicality::Temporary,
                    ret_type,
                ))
            }

            label if label == noteq_label => {
                let arg2 = function_arguments.pop().unwrap();
                let arg1 = function_arguments.pop().unwrap();

                let typ = self.type_resolver.fetch_final_ir_type(template_types[0]);
                let Some(typ) = typ else {
                    return Err(CompilerError {
                        message: "Template type should be known by now".to_string(),
                        position: Some(call_pos),
                    });
                };

                match typ {
                    IRType::Primitive(IRPrimitiveType::I32) |
                    IRType::Primitive(IRPrimitiveType::I64) |
                    IRType::Primitive(IRPrimitiveType::F32) |
                    IRType::Primitive(IRPrimitiveType::F64)
                    => {} // ok
                    _ => return Err(CompilerError {
                        message: format!("Builtin operator does not support {typ:?}"),
                        position: Some(call_pos),
                    })
                }

                self.type_resolver.hint_equal(expr_types[0], template_types[0])?;
                self.type_resolver.hint_equal(expr_types[1], template_types[0])?;

                let ret_type = self.type_resolver.new_type_label(FilePosition::unknown());
                self.type_resolver.hint_is(ret_type, IRPrimitiveType::Bool)?;

                Ok((
                    BuiltinFunctionCall::NotEq {
                        arg1: Box::new(arg1),
                        arg2: Box::new(arg2),
                    },
                    ValuePhysicality::Temporary,
                    ret_type,
                ))
            }

            _ => unreachable!(),
        }
    }
}
