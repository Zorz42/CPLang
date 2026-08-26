// this replaces all void instances with empty struct, since void variables do not compile in c

use crate::compiler::normalizer::ir::{IR, IRExpression, IRStatement, IRStruct, IRStructLabel, IRType};
use crate::compiler::normalizer::ir_pass::IRPass;
use crate::compiler::parser::ast::PrimitiveType;

fn process_type(typ: IRType, void_struct: IRStructLabel) -> IRType {
    match typ {
        IRType::Primitive(PrimitiveType::Void) => IRType::Struct(void_struct, Vec::new()),
        IRType::Primitive(prim) => IRType::Primitive(prim),
        IRType::Reference(typ) => IRType::Reference(Box::new(process_type(*typ, void_struct))),
        IRType::Struct(label, fields) =>
            IRType::Struct(label, fields.into_iter().map(|x| process_type(x, void_struct)).collect())
    }
}

// we still need the pass to modify all return statements of void function to return struct instead
struct VoidPass {
    void_struct: IRStructLabel,
}

impl IRPass for VoidPass {
    fn pre_map_statement(&mut self, statement: IRStatement) -> IRStatement {
        match statement {
            IRStatement::Return { return_value: None } =>
                IRStatement::Return {
                    return_value: Some(IRExpression::StructInitialization {
                        struct_label: self.void_struct,
                        fields_type_labels: Vec::new(),
                        field_values: Vec::new(),
                    })
                },
            _ => statement,
        }
    }
}

pub fn process_void_variables(mut ir: IR) -> IR {
    let void_struct: IRStructLabel = ir.structs.len();
    ir.structs.push(IRStruct { fields: Vec::new() });

    ir.types = ir.types.into_iter().map(|(key, val)| {
        (key, process_type(val, void_struct))
    }).collect();

    let mut pass = VoidPass { void_struct };
    pass.pass_ir(ir)
}
