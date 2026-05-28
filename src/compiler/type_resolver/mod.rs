use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::normalizer::ir::{IRAutoRefLabel, IRFieldLabel, IRPrimitiveType, IRStructLabel, IRType, IRTypeLabel};
use crate::compiler::type_resolver::dsu::Dsu;
use std::collections::{HashMap, HashSet, VecDeque};
use std::mem::swap;
use std::ops::Add;

mod compare_sets;
pub mod dsu;
mod test;

#[derive(Default, Clone)]
pub struct Node {
    parent_structs: Vec<(IRTypeLabel, IRFieldLabel)>,
    ref_depth: i32,
    in_queue: bool,
}

impl Add for Node {
    type Output = Self;

    fn add(mut self, mut rhs: Self) -> Self::Output {
        self.in_queue = self.in_queue || rhs.in_queue;
        if self.parent_structs.len() < rhs.parent_structs.len() {
            swap(&mut self.parent_structs, &mut rhs.parent_structs);
        }
        self.parent_structs.append(&mut rhs.parent_structs);
        self
    }
}

#[derive(Default, Clone)]
pub struct TypeNode {
    typ: Option<IRType>,
    child_fields: HashMap<IRFieldLabel, IRTypeLabel>,
    known_struct: Option<IRStructLabel>,
    // stores all reference values of nodes
    // (representative, ref_depth) -> node
    // if two nodes have the same representative and ref_depth (within type component),
    // they are the same exact type and should be merged
    // representative is from ref_dsu
    ref_map: HashMap<(IRTypeLabel, i32), IRTypeLabel>,
    // a vector of types it is equal to (gets merged in queue phase)
    // it is delayed to avoid recursion
    to_merge: Vec<IRTypeLabel>,
}

impl Add for TypeNode {
    type Output = Self;

    fn add(mut self, mut rhs: Self) -> Self::Output {
        assert_eq!(self.known_struct, rhs.known_struct);
        if !rhs.child_fields.is_empty() {
            swap(&mut self.child_fields, &mut rhs.child_fields);
        }
        assert!(rhs.child_fields.is_empty());
        assert_eq!(self.typ, rhs.typ);
        if !rhs.ref_map.is_empty() {
            swap(&mut rhs.ref_map, &mut self.ref_map);
        }
        assert!(rhs.ref_map.is_empty());
        self
    }
}

#[derive(Default, Clone)]
pub struct RefNode {
    nodes: Vec<IRTypeLabel>,
}

impl Add for RefNode {
    type Output = Self;

    fn add(mut self, mut rhs: Self) -> Self::Output {
        if self.nodes.len() < rhs.nodes.len() {
            swap(&mut self.nodes, &mut rhs.nodes);
        }
        self.nodes.append(&mut rhs.nodes);
        self
    }
}

#[derive(Clone, Default)]
pub struct TypeResolver {
    type_positions: Vec<FilePosition>,
    queue: VecDeque<IRTypeLabel>,
    auto_ref_pairs: Vec<(IRTypeLabel, IRTypeLabel)>,
    // two labels are in the same component if they have exactly the same type
    dsu: Dsu<Node>,
    // two labels are in the same component if they have the same type (up to reference)
    type_dsu: Dsu<TypeNode>,
    // two labels are in the same component if they have dependant reference count
    ref_dsu: Dsu<RefNode>,
    // component that has fixed ref value (when you set concrete ref value)
    fixed_ref_component: usize,
    // already known types (up to reference) can be merged if they are the same
    type_map: HashMap<IRType, IRTypeLabel>,
    // static data about struct fields
    structs: Vec<HashSet<IRFieldLabel>>,
    structs_ord: Vec<Vec<IRFieldLabel>>,
}

impl TypeResolver {
    pub fn new(structs: Vec<Vec<IRFieldLabel>>) -> Self {
        let mut res = Self {
            structs_ord: structs.clone(),
            structs: structs.into_iter().map(HashSet::from_iter).collect(),
            ..Default::default()
        };
        let fixed = res.new_type_label(FilePosition::unknown());
        res.fixed_ref_component = fixed;
        res.type_dsu.get(res.fixed_ref_component).typ = Some(IRType::Primitive(IRPrimitiveType::Void));
        res
    }

    pub fn get_structs(&self) -> Vec<Vec<IRFieldLabel>> {
        self.structs_ord.clone()
    }

    fn get_num_known_fields(&mut self, type_label: IRTypeLabel) -> usize {
        let mut num_known_fields = 0;
        let values = self.type_dsu.get(type_label).child_fields.values().copied().collect::<Vec<_>>();
        for field_type_label in values {
            if self.check_is_type_known(field_type_label) {
                num_known_fields += 1;
            }
        }
        num_known_fields
    }

    pub fn new_type_label(&mut self, pos: FilePosition) -> IRTypeLabel {
        let res = self.dsu.len() as IRTypeLabel;
        self.dsu.add();
        self.type_dsu.add();
        self.type_dsu.get(res).ref_map.insert((res, 0), res);
        self.ref_dsu.add();
        self.ref_dsu.get(res).nodes.push(res);
        self.type_positions.push(pos);
        res
    }

    fn add_to_queue(&mut self, label: IRTypeLabel) {
        if self.dsu.get(label).in_queue {
            return;
        }
        self.dsu.get(label).in_queue = true;
        self.queue.push_back(label);
    }

    pub fn new_autoref_label(&mut self, label1: IRTypeLabel, label2: IRTypeLabel) -> IRAutoRefLabel {
        let res = self.auto_ref_pairs.len() as IRAutoRefLabel;
        self.auto_ref_pairs.push((label1, label2));
        res
    }

    pub fn fetch_final_ir_type(&mut self, label: IRTypeLabel) -> Option<IRType> {
        let dep = self.dsu.get(label).ref_depth;
        if dep < 0 || !self.ref_is_fixed(label) {
            return None;
        }
        let mut typ = self.type_dsu.get(label).typ.clone()?;

        for _ in 0..dep {
            typ = IRType::Reference(Box::new(typ));
        }
        Some(typ)
    }

    fn ref_is_fixed(&mut self, label: IRTypeLabel) -> bool {
        self.ref_dsu.get_repr(self.fixed_ref_component) == self.ref_dsu.get_repr(label)
    }

    // push to queue every struct type that depends on label's type (in type_dsu)
    fn push_type_parents(&mut self, label: IRTypeLabel) {
        let mut to_queue = Vec::new();
        for label2 in self.type_dsu.get(label).ref_map.values() {
            for (parent_type, _field_label) in &self.dsu.get(*label2).parent_structs {
                to_queue.push(*parent_type);
            }
        }
        for i in to_queue {
            self.add_to_queue(i);
        }
    }

    fn set_type(&mut self, label: IRTypeLabel, typ: IRType) -> CompilerResult<()> {
        if self.type_dsu.get(label).typ.is_some() && self.type_dsu.get(label).typ.as_ref() != Some(&typ) {
            return Err(CompilerError {
                message: format!(
                    "This cannot be of type {:?} and {:?} at the same time",
                    self.type_dsu.get(label).typ.as_ref().unwrap(),
                    typ
                ),
                position: Some(self.type_positions[label]),
            });
        }
        if self.type_dsu.get(label).typ.is_some() {
            return Ok(());
        }
        if let Some(label2) = self.type_map.get(&typ) {
            self.schedule_merge_type(label, *label2);
        } else {
            self.type_map.insert(typ.clone(), label);
            self.type_dsu.get(label).typ = Some(typ);
            let labels = self.type_dsu.get(label).ref_map.values().copied().collect::<Vec<_>>();
            for label in labels {
                self.add_to_queue(label);
            }
        }

        self.push_type_parents(label);

        Ok(())
    }

    fn set_ref(&mut self, label: IRTypeLabel, ref_val: i32) -> CompilerResult<()> {
        self.merge_ref(self.fixed_ref_component, label, ref_val)
    }

    fn run_queue(&mut self) -> CompilerResult<()> {
        while let Some(node) = self.queue.pop_front() {
            self.dsu.get(node).in_queue = false;

            // when a node is updated it does the following:
            // - it merges with all neighbouring nodes (it is delayed until this stage)
            // - in case it is a struct type: it checks if all its child fields are known, then the type is automatically known

            let mut to_merge = Vec::new();
            swap(&mut to_merge, &mut self.type_dsu.get(node).to_merge);
            for node2 in to_merge {
                if self.type_dsu.get_repr(node) == self.type_dsu.get_repr(node2) {
                    continue;
                }

                self.merge_type(node, node2)?;
            }
            let num_known_fields = self.get_num_known_fields(node);

            if let Some(struct_label) = self.type_dsu.get(node).known_struct
                && num_known_fields == self.structs_ord[struct_label].len()
                && self.ref_is_fixed(node)
                && self.dsu.get(node).ref_depth == 0
            {
                let mut struct_fields = Vec::new();
                for field_label in self.structs_ord[struct_label].clone() {
                    let type_label = self.type_dsu.get(node).child_fields[&field_label];
                    struct_fields.push(self.fetch_final_ir_type(type_label).unwrap());
                }
                self.set_type(node, IRType::Struct(struct_label, struct_fields))?;
            }
        }

        Ok(())
    }

    fn merge(&mut self, label1: IRTypeLabel, label2: IRTypeLabel) -> CompilerResult<()> {
        self.schedule_merge_type(label1, label2);
        self.merge_ref(label1, label2, 0)?;

        Ok(())
    }

    fn schedule_merge_type(&mut self, label1: IRTypeLabel, label2: IRTypeLabel) {
        self.type_dsu.get(label1).to_merge.push(label2);
        self.add_to_queue(label1);
    }

    fn merge_type(&mut self, label1: IRTypeLabel, label2: IRTypeLabel) -> CompilerResult<()> {
        let mut typ1 = self.type_dsu.get(label1).typ.clone();
        let mut typ2 = self.type_dsu.get(label2).typ.clone();

        if typ1.is_none() {
            typ1 = typ2.clone();
        } else if typ2.is_none() {
            typ2 = typ1.clone();
        } else if typ1 != typ2 {
            #[allow(clippy::unnecessary_unwrap)]
            return Err(CompilerError {
                message: format!("This expression cannot be {:?} and {:?} at the same time.", typ1.unwrap(), typ2.unwrap()),
                position: Some(self.type_positions[label1]),
            });
        }

        if self.type_dsu.get(label1).typ != typ1 {
            self.type_dsu.get(label1).typ = typ1;
            // go through all different types in that component
            let labels = self.type_dsu.get(label1).ref_map.values().copied().collect::<Vec<_>>();
            for label in labels {
                self.add_to_queue(label);
            }
        }
        if self.type_dsu.get(label2).typ != typ2 {
            self.type_dsu.get(label2).typ = typ2;
            let labels = self.type_dsu.get(label2).ref_map.values().copied().collect::<Vec<_>>();
            for label in labels {
                self.add_to_queue(label);
            }
        }

        // merge known_struct
        if let Some(s1) = self.type_dsu.get(label1).known_struct
            && let Some(s2) = self.type_dsu.get(label2).known_struct
            && s1 != s2
        {
            return Err(CompilerError {
                message: "This type cannot be two different struct types at the same time.".to_string(),
                position: Some(self.type_positions[label1]),
            });
        }

        if self.type_dsu.get(label1).known_struct.is_none() {
            self.type_dsu.get(label1).known_struct = self.type_dsu.get(label2).known_struct;
        } else {
            self.type_dsu.get(label2).known_struct = self.type_dsu.get(label1).known_struct;
        }

        // merge child fields
        for (field_label, field_type) in self.type_dsu.get(label2).child_fields.clone() {
            if let Some(field_type2) = self.type_dsu.get(label1).child_fields.get(&field_label).copied() {
                self.merge(field_type, field_type2)?;
            } else {
                self.type_dsu.get(label1).child_fields.insert(field_label, field_type);
            }
        }
        self.type_dsu.get(label2).child_fields.clear();

        // merge same refs
        let mut ref_map = HashMap::new();
        swap(&mut ref_map, &mut self.type_dsu.get(label1).ref_map);
        for (key, label) in ref_map {
            if let Some(other_label) = self.type_dsu.get(label2).ref_map.get(&key).copied() {
                if self.dsu.get_repr(label) != self.dsu.get_repr(other_label) {
                    self.add_to_queue(label);
                    self.dsu.merge(label, other_label);
                }
            } else {
                self.type_dsu.get(label2).ref_map.insert(key, label);
            }
        }

        // push refs into queue

        self.type_dsu.merge(label1, label2);

        if let Some(typ) = &self.type_dsu.get(label1).typ {
            let known_struct = match typ {
                IRType::Struct(label, _) => Some(*label),
                _ => None,
            };

            if self.type_dsu.get(label1).known_struct != known_struct {
                return Err(CompilerError {
                    message: "This type cannot be two different struct types at the same time.".to_string(),
                    position: Some(self.type_positions[label1]),
                });
            }
        }

        self.push_type_parents(label1);

        Ok(())
    }

    // label2 has exactly offset more references than label1
    fn merge_ref(&mut self, label1: IRTypeLabel, label2: IRTypeLabel, offset: i32) -> CompilerResult<()> {
        if self.ref_dsu.get_repr(label1) == self.ref_dsu.get_repr(label2) {
            // already merged, just check if there is no contradiction
            if self.dsu.get(label1).ref_depth + offset != self.dsu.get(label2).ref_depth {
                return Err(CompilerError {
                    message: format!(
                        "This cannot be {}-time reference and {}-time reference at the same time",
                        self.dsu.get(label1).ref_depth + offset,
                        self.dsu.get(label2).ref_depth
                    ),
                    position: Some(self.type_positions[label2]),
                });
            }
            return Ok(());
        }

        let mut all_nodes = self.ref_dsu.get(label1).nodes.clone();
        all_nodes.append(&mut self.ref_dsu.get(label2).nodes.clone());

        for node in all_nodes {
            let key = (self.ref_dsu.get_repr(node), self.dsu.get(node).ref_depth);
            self.type_dsu.get(node).ref_map.remove(&key);
        }

        let offset = offset + self.dsu.get(label1).ref_depth - self.dsu.get(label2).ref_depth;
        let (to_modify, modify_offset) = if self.ref_is_fixed(label2) {
            (self.ref_dsu.get(label1).nodes.clone(), -offset)
        } else {
            (self.ref_dsu.get(label2).nodes.clone(), offset)
        };

        let mut updated_nodes = HashSet::new();
        for node in to_modify {
            let repr = self.dsu.get_repr(node);
            if updated_nodes.contains(&repr) {
                continue;
            }
            updated_nodes.insert(repr);

            self.dsu.get(node).ref_depth += modify_offset;
        }

        if self.ref_dsu.merge(label1, label2) {
            self.add_to_queue(label1);
            self.add_to_queue(label2);
        }

        // update ref_dsu's ref_maps
        all_nodes = self.ref_dsu.get(label1).nodes.clone();
        for node in all_nodes {
            let key = (self.ref_dsu.get_repr(node), self.dsu.get(node).ref_depth);
            if let Some(label) = self.type_dsu.get(node).ref_map.get(&key).copied() {
                if self.dsu.get_repr(label) != self.dsu.get_repr(node) {
                    self.add_to_queue(label);
                    self.dsu.merge(label, node);
                }
            } else {
                self.type_dsu.get(node).ref_map.insert(key, node);
            }
        }

        self.push_type_parents(label1);

        Ok(())
    }

    pub fn hint_is(&mut self, label: IRTypeLabel, typ: IRPrimitiveType) -> CompilerResult<()> {
        #[cfg(feature = "trace")]
        println!("hint_is({label}, {typ:?})");

        self.set_type(label, IRType::Primitive(typ))?;
        self.set_ref(label, 0)?;

        self.run_queue()?;

        Ok(())
    }

    pub fn hint_equal(&mut self, label1: IRTypeLabel, label2: IRTypeLabel) -> CompilerResult<()> {
        #[cfg(feature = "trace")]
        println!("hint_equal({label1}, {label2})");
        self.merge(label1, label2)?;

        self.run_queue()?;
        Ok(())
    }

    pub fn hint_is_ref(&mut self, phys_label: IRTypeLabel, ref_label: IRTypeLabel) -> CompilerResult<()> {
        #[cfg(feature = "trace")]
        println!("hint_is_ref({phys_label}, {ref_label})");
        self.schedule_merge_type(phys_label, ref_label);
        self.merge_ref(phys_label, ref_label, 1)?;

        self.run_queue()?;
        Ok(())
    }

    fn set_field(&mut self, field_type_label: IRTypeLabel, field_label: IRFieldLabel, struct_type_label: IRTypeLabel) -> CompilerResult<()> {
        #[cfg(feature = "trace")]
        println!("set field field_type={field_type_label} field_label={field_label} struct_type={struct_type_label}");
        if let Some(field_label) = self.type_dsu.get(struct_type_label).child_fields.get(&field_label).copied() {
            self.merge(field_label, field_type_label)?;
        } else {
            self.type_dsu.get(struct_type_label).child_fields.insert(field_label, field_type_label);
        }

        self.dsu.get(field_type_label).parent_structs.push((struct_type_label, field_label));
        self.add_to_queue(struct_type_label);

        Ok(())
    }

    pub fn hint_struct(&mut self, struct_type_label: IRTypeLabel, struct_label: IRStructLabel, field_type_labels: Vec<IRTypeLabel>) -> CompilerResult<()> {
        #[cfg(feature = "trace")]
        println!("hint_struct({struct_type_label}, {struct_label}, {field_type_labels:?})");
        assert_eq!(field_type_labels.len(), self.structs[struct_label].len());

        if let Some(curr_struct_label) = self.type_dsu.get(struct_type_label).known_struct {
            if curr_struct_label != struct_label {
                return Err(CompilerError {
                    message: "This type cannot be two different struct types at the same time.".to_string(),
                    position: Some(self.type_positions[struct_type_label]),
                });
            }
        } else {
            self.type_dsu.get(struct_type_label).known_struct = Some(struct_label);
        }

        for (field_label, field_type) in self.structs_ord[struct_label].clone().into_iter().zip(field_type_labels) {
            assert!(self.structs[struct_label].contains(&field_label));

            self.set_field(field_type, field_label, struct_type_label)?;
        }

        self.set_ref(struct_type_label, 0)?;

        self.run_queue()?;
        Ok(())
    }

    pub fn hint_is_field(&mut self, field_type_label: IRTypeLabel, struct_type_label: IRTypeLabel, field_label: IRFieldLabel) -> CompilerResult<()> {
        #[cfg(feature = "trace")]
        println!("hint_is_field({field_type_label}, {struct_type_label}, {field_label})");
        self.set_field(field_type_label, field_label, struct_type_label)?;

        self.run_queue()?;
        Ok(())
    }

    pub fn hint_autoref(&mut self, label1: IRTypeLabel, label2: IRTypeLabel) -> CompilerResult<()> {
        #[cfg(feature = "trace")]
        println!("hint_autoref({label1}, {label2})");
        self.schedule_merge_type(label1, label2);

        self.run_queue()?;
        Ok(())
    }

    pub fn hint_is_phys(&mut self, label: IRTypeLabel) -> CompilerResult<()> {
        #[cfg(feature = "trace")]
        println!("hint_is_phys({label})");
        self.set_ref(label, 0)?;

        self.run_queue()?;
        Ok(())
    }

    pub fn gather_types(mut self, needed_types: Vec<IRTypeLabel>) -> CompilerResult<(HashMap<IRTypeLabel, IRType>, Vec<i32>)> {
        let mut types = HashMap::new();
        let mut autorefs = vec![0; self.auto_ref_pairs.len()];

        /*for i in 0..self.dsu.len() {
            print!("{} {:?} | ", self.dsu.get(i).ref_depth, self.type_dsu.get(i).typ);
        }
        println!();*/

        // go through all ref pairs and naiively match them by reference depth if not both sides are determined
        for (mut type1, mut type2) in self.auto_ref_pairs.clone() {
            if self.ref_dsu.get_repr(type1) != self.ref_dsu.get_repr(type2) {
                let mut diff = 0;
                if self.ref_is_fixed(type1) || self.ref_is_fixed(type2) {
                    if self.ref_is_fixed(type2) {
                        swap(&mut type1, &mut type2);
                    }
                    for node in self.ref_dsu.get(type2).nodes.clone() {
                        diff = i32::min(diff, self.dsu.get(node).ref_depth);
                    }
                }

                self.merge_ref(type1, type2, -diff)?;
                self.run_queue()?;
            }
        }


        for (i, (type1, type2)) in self.auto_ref_pairs.iter().enumerate() {
            autorefs[i] = self.dsu.get(*type1).ref_depth - self.dsu.get(*type2).ref_depth;
        }

        for type_label in needed_types {
            let Some(typ) = self.fetch_final_ir_type(type_label) else {
                return Err(CompilerError {
                    message: "Could not deduce this expression's type".to_string(),
                    position: Some(self.type_positions[type_label]),
                });
            };

            types.insert(type_label, typ);
        }

        Ok((types, autorefs))
    }

    pub fn are_equal(&mut self, label1: IRTypeLabel, label2: IRTypeLabel) -> bool {
        self.dsu.get_repr(label1) == self.dsu.get_repr(label2)
    }

    pub fn check_is_type_known(&mut self, label: IRTypeLabel) -> bool {
        self.type_dsu.get(label).typ.is_some() && self.ref_is_fixed(label)
    }
}
