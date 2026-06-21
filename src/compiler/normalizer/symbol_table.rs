use crate::compiler::normalizer::ir::{IRFieldLabel, IRInstanceLabel, IRStructLabel, IRVariableLabel};
use std::collections::HashMap;
use std::mem::swap;

// change in scope regarding variables
enum ScopeChange {
    AddedVariable(String),
    OverridenVariable(String, IRVariableLabel),
}

// symbols are of type: variable, function, struct
#[derive(Default)]
pub struct SymbolTable {
    global_variable_name_map: HashMap<String, IRVariableLabel>,
    curr_variable_label: IRVariableLabel,
    scopes: Vec<Vec<ScopeChange>>,
    variable_name_map: HashMap<String, IRVariableLabel>,

    // struct fields
    curr_field_label: IRFieldLabel,
    fields_name_map: HashMap<String, IRFieldLabel>,

    curr_struct_label: IRStructLabel,
    structs_name_map: HashMap<String, IRStructLabel>,

    curr_instance_label: IRInstanceLabel,
}

#[derive(Default)]
pub struct FunctionSymbolData {
    scopes: Vec<Vec<ScopeChange>>,
    variable_name_map: HashMap<String, IRVariableLabel>,
}

impl SymbolTable {
    pub fn push_scope(&mut self) {
        self.scopes.push(Vec::new());
    }

    pub fn pop_scope(&mut self) {
        let v = self.scopes.pop().unwrap();

        for change in v.into_iter().rev() {
            match change {
                ScopeChange::AddedVariable(name) => self.variable_name_map.remove(&name),
                ScopeChange::OverridenVariable(name, label) => self.variable_name_map.insert(name, label),
            };
        }
    }

    pub fn push_function(&mut self) -> FunctionSymbolData {
        let mut data = FunctionSymbolData::default();
        swap(&mut data.scopes, &mut self.scopes);
        swap(&mut data.variable_name_map, &mut self.variable_name_map);
        self.variable_name_map = self.global_variable_name_map.clone();
        self.push_scope();
        data
    }

    pub fn pop_function(&mut self, mut data: FunctionSymbolData) {
        swap(&mut data.scopes, &mut self.scopes);
        swap(&mut data.variable_name_map, &mut self.variable_name_map);
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

    pub fn get_field(&self, name: &str) -> Option<IRFieldLabel> {
        self.fields_name_map.get(name).copied()
    }

    pub const fn new_instance_label(&mut self) -> IRInstanceLabel {
        self.curr_instance_label += 1;
        self.curr_instance_label - 1
    }

    pub fn new_struct(&mut self, name: String) -> IRStructLabel {
        let label = self.curr_struct_label;
        self.structs_name_map.insert(name, label);
        self.curr_struct_label += 1;
        label
    }

    pub fn get_struct_label(&self, name: &str) -> Option<IRStructLabel> {
        self.structs_name_map.get(name).copied()
    }

    pub fn new_variable(&mut self, name: &str) -> IRVariableLabel {
        let label = self.curr_variable_label;
        self.curr_variable_label += 1;
        let event = self.variable_name_map.get(name).map_or_else(
            || ScopeChange::AddedVariable(name.to_string()),
            |label| ScopeChange::OverridenVariable(name.to_string(), *label),
        );
        self.scopes.last_mut().unwrap().push(event);
        self.variable_name_map.insert(name.to_string(), label);
        label
    }

    pub fn new_global_variable(&mut self, name: &str) -> IRVariableLabel {
        let label = self.curr_variable_label;
        self.curr_variable_label += 1;
        self.global_variable_name_map.insert(name.to_string(), label);
        label
    }

    pub fn get_variable(&self, name: &str) -> Option<IRVariableLabel> {
        self.variable_name_map.get(name).copied()
    }
}
