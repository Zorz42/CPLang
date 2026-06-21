use crate::compiler::normalizer::ir::{IRFieldLabel, IRInstanceLabel, IRStructLabel, IRVariableLabel};
use crate::compiler::parser::ast::{ASTBlock, ASTFunctionSignature};
use std::collections::HashMap;

// symbols are of type: variable, function, struct
#[derive(Default)]
pub struct SymbolTable {
    variable_name_map: HashMap<String, IRVariableLabel>,
    // key is (function name, number of arguments)
    functions_name_map: HashMap<(String, usize), Vec<(ASTFunctionSignature, ASTBlock)>>,
    // struct fields
    curr_field_label: IRFieldLabel,
    fields_name_map: HashMap<String, IRFieldLabel>,

    curr_struct_label: IRStructLabel,
    structs_name_map: HashMap<String, IRStructLabel>,

    curr_instance_label: IRInstanceLabel,
}

impl SymbolTable {
    pub fn push_scope(&mut self) {
        todo!()
    }

    pub fn pop_scope(&mut self) {
        todo!()
    }

    pub fn create_field(&mut self, name: String) -> IRFieldLabel {
        if let Some(label) = self.fields_name_map.get(&name) {
            *label
        } else {
            let label = self.curr_field_label;
            self.curr_field_label += 1;
            self.fields_name_map.insert(name, label);
            label
        }
    }

    pub fn get_field(&mut self, name: &str) -> Option<IRFieldLabel> {
        self.fields_name_map.get(name).map(|x| *x)
    }

    pub fn new_instance_label(&mut self) -> IRInstanceLabel {
        self.curr_instance_label += 1;
        self.curr_instance_label - 1
    }

    pub fn new_struct(&mut self, name: String) -> IRStructLabel {
        let label = self.curr_struct_label;
        self.structs_name_map.insert(name, label);
        self.curr_struct_label += 1;
        label
    }

    pub fn get_struct_label(&mut self, name: &str) -> Option<IRStructLabel> {
        self.structs_name_map.get(name).map(|x| *x)
    }
}