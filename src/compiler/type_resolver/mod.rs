use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::normalizer::ir::{IRAutoRefLabel, IRFieldLabel, IRPrimitiveType, IRStructLabel, IRType, IRTypeLabel};
use crate::compiler::type_resolver::dsu::Dsu;
use std::collections::{HashMap, HashSet, VecDeque};
use std::mem::swap;
use std::ops::Add;

pub mod dsu;
mod compare_sets;
mod test;

#[derive(Clone)]
enum Conn {
    // arg1 = arg2 { arg3[0], ..., arg3[n] }
    Struct(IRTypeLabel, IRStructLabel, Vec<IRTypeLabel>),

    // arg1 = node.arg2
    IsField(IRTypeLabel, IRFieldLabel),
}

fn deref_type(mut ir_type: IRType) -> (IRType, i32) {
    let mut ref_depth = 0;

    while let IRType::Reference(inner) = ir_type {
        ir_type = *inner;
        ref_depth += 1;
    }

    (ir_type, ref_depth)
}

#[derive(Default, Clone)]
pub struct Node {
    neighbours: Vec<Conn>,
    ref_depth: i32,
}

impl Add for Node {
    type Output = Self;

    fn add(mut self, mut rhs: Self) -> Self::Output {
        self.neighbours.append(&mut rhs.neighbours);
        self
    }
}

#[derive(Default, Clone)]
pub struct TypeNode {
    typ: Option<IRType>,
    // stores all reference values of nodes
    // (representative, ref_depth) -> node
    // if two nodes have the same representative and ref_depth (within type component),
    // they are the same exact type and should be merged
    // representative is from ref_dsu
    ref_map: HashMap<(IRTypeLabel, i32), IRTypeLabel>,
}

impl Add for TypeNode {
    type Output = Self;

    fn add(mut self, mut rhs: Self) -> Self::Output {
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
    structs: Vec<Vec<IRFieldLabel>>,
}

impl TypeResolver {
    pub fn new(structs: Vec<Vec<IRFieldLabel>>) -> Self {
        let mut res = Self {
            structs,
            ..Default::default()
        };
        let fixed = res.new_type_label(FilePosition::unknown());
        res.fixed_ref_component = fixed;
        res.type_dsu.get(res.fixed_ref_component).typ = Some(IRType::Primitive(IRPrimitiveType::Void));
        res
    }

    pub fn get_structs(&self) -> Vec<Vec<IRFieldLabel>> {
        self.structs.clone()
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

    pub fn new_autoref_label(&mut self, label1: IRTypeLabel, label2: IRTypeLabel) -> IRAutoRefLabel {
        let res = self.auto_ref_pairs.len() as IRAutoRefLabel;
        self.auto_ref_pairs.push((label1, label2));
        res
    }

    pub fn fetch_final_ir_type(&mut self, label: IRTypeLabel) -> Option<IRType> {
        let mut typ = self.type_dsu.get(label).typ.clone()?;
        let dep = self.dsu.get(label).ref_depth;
        if dep < 0 || !self.ref_is_fixed(label) {
            return None;
        }

        for _ in 0..dep {
            typ = IRType::Reference(Box::new(typ));
        }
        Some(typ)
    }

    fn ref_is_fixed(&mut self, label: IRTypeLabel) -> bool {
        self.ref_dsu.get_repr(self.fixed_ref_component) == self.ref_dsu.get_repr(label)
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
        if self.type_dsu.get(label).typ.is_none() {
            if let Some(label2) = self.type_map.get(&typ) {
                self.merge_type(label, *label2)?;
            } else {
                self.type_map.insert(typ.clone(), label);
                self.type_dsu.get(label).typ = Some(typ);
                for label in self.type_dsu.get(label).ref_map.values() {
                    self.queue.push_back(*label);
                }
            }
        }
        Ok(())
    }

    fn set_ref(&mut self, label: IRTypeLabel, ref_val: i32) -> CompilerResult<()> {
        self.merge_ref(self.fixed_ref_component, label, ref_val)
    }

    fn run_queue(&mut self) -> CompilerResult<()> {
        while let Some(node) = self.queue.pop_front() {
            // loop for type deduction
            if let Some(node_type) = self.type_dsu.get(node).typ.clone() {
                'neighbour_loop: for ne in self.dsu.get(node).neighbours.clone() {
                    match ne {
                        Conn::Struct(typ, structure, args) => {
                            if typ == node {
                                let IRType::Struct(_, args2) = node_type.clone() else {
                                    return Err(CompilerError {
                                        message: format!("This cannot be a struct and {node_type:?} at the same time."),
                                        position: Some(self.type_positions[node]),
                                    });
                                };

                                for (typ_label, typ) in args.into_iter().zip(args2) {
                                    let (typ, ref_depth) = deref_type(typ);
                                    self.set_type(typ_label, typ)?;
                                    self.set_ref(typ_label, ref_depth)?;
                                }

                                continue 'neighbour_loop;
                            }

                            // only deduce type when all arguments are known
                            let mut ir_args = Vec::new();
                            for arg in args {
                                if let Some(typ) = self.fetch_final_ir_type(arg) {
                                    ir_args.push(typ);
                                } else {
                                    continue 'neighbour_loop;
                                }
                            }

                            let ir_type = IRType::Struct(structure, ir_args);
                            self.set_type(typ, ir_type)?;
                        }
                        Conn::IsField(typ, field) => {
                            let (struct_label, struct_args) = match &self.type_dsu.get(node).typ {
                                Some(IRType::Struct(label, args)) => (*label, args),
                                _ => {
                                    return Err(CompilerError {
                                        message: format!(
                                            "Cannot access field since this is not a struct but has type {:?}",
                                            self.type_dsu.get(node).typ.as_ref().unwrap()
                                        ),
                                        position: Some(self.type_positions[node]),
                                    });
                                }
                            };
                            let curr_struct = &self.structs[struct_label];
                            let mut field_idx = 0;
                            for (idx, curr_field) in curr_struct.iter().enumerate() {
                                if *curr_field == field {
                                    field_idx = idx;
                                }
                            }

                            let ir_type = struct_args[field_idx].clone();
                            let (ir_type, ref_depth) = deref_type(ir_type);

                            self.set_type(typ, ir_type)?;
                            self.set_ref(typ, ref_depth)?;
                        }
                    }
                }
            }

            if self.type_dsu.get(node).typ.is_none() {
                // if the type is struct, save its fields (if they exist).
                let mut struct_fields: Option<(IRStructLabel, Vec<IRTypeLabel>)> = None;

                for ne in self.dsu.get(node).neighbours.clone() {
                    if let Conn::Struct(typ, structure, args) = ne
                        && self.are_equal(typ, node)
                    {
                        if let Some((curr_structure, curr_args)) = &struct_fields {
                            assert_eq!(*curr_structure, structure);

                            for (arg1, arg2) in args.iter().zip(curr_args.clone()) {
                                self.merge(*arg1, arg2)?;
                            }
                        } else {
                            struct_fields = Some((structure, args.clone()));
                        }
                    }
                }

                let mut fields = HashMap::new();

                if let Some((structure, args)) = &struct_fields {
                    for (arg, field_label) in args.iter().zip(&self.structs[*structure]) {
                        fields.insert(*field_label, *arg);
                    }
                }

                for ne in self.dsu.get(node).neighbours.clone() {
                    if let Conn::IsField(typ, field) = ne {
                        if let Some(typ2) = fields.get(&field) {
                            self.merge(typ, *typ2)?;
                        } else {
                            fields.insert(field, typ);
                        }
                    }
                }

                // check if all field types are now known
                if let Some((structure, args)) = struct_fields {
                    let mut arg_types = Vec::new();

                    for arg in &args {
                        if let Some(typ) = &self.type_dsu.get(*arg).typ {
                            arg_types.push(typ.clone());
                        } else {
                            break;
                        }
                    }

                    if arg_types.len() == args.len() {
                        self.type_dsu.get(node).typ = Some(IRType::Struct(structure, arg_types));
                    }
                }
            }
        }

        Ok(())
    }

    fn merge(&mut self, label1: IRTypeLabel, label2: IRTypeLabel) -> CompilerResult<()> {
        self.merge_type(label1, label2)?;
        self.merge_ref(label1, label2, 0)?;

        Ok(())
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
            for label in self.type_dsu.get(label1).ref_map.values() {
                self.queue.push_back(*label);
            }
        }
        if self.type_dsu.get(label2).typ != typ2 {
            self.type_dsu.get(label2).typ = typ2;
            for label in self.type_dsu.get(label2).ref_map.values() {
                self.queue.push_back(*label);
            }
        }

        let mut ref_map = HashMap::new();
        swap(&mut ref_map, &mut self.type_dsu.get(label1).ref_map);
        for (key, label) in ref_map {
            if let Some(other_label) = self.type_dsu.get(label2).ref_map.get(&key).copied() {
                if self.dsu.merge(label, other_label) {
                    self.queue.push_back(label);
                }
            } else {
                self.type_dsu.get(label2).ref_map.insert(key, label);
            }
        }

        self.type_dsu.merge(label1, label2);

        Ok(())
    }

    // label2 has offset more references than label1
    fn merge_ref(&mut self, label1: IRTypeLabel, label2: IRTypeLabel, offset: i32) -> CompilerResult<()> {
        if self.ref_dsu.get_repr(label1) == self.ref_dsu.get_repr(label2) {
            // already merged, just check if there is no contradiction
            if self.dsu.get(label1).ref_depth + offset != self.dsu.get(label2).ref_depth {
                return Err(CompilerError {
                    message: format!(
                        "This cannot be {}-time reference and {}-time reference at the same time",
                        self.dsu.get(label2).ref_depth - offset,
                        self.dsu.get(label1).ref_depth
                    ),
                    position: Some(self.type_positions[label1]),
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
            self.queue.push_back(label1);
            self.queue.push_back(label2);
        }

        // update ref_dsu's ref_maps
        all_nodes = self.ref_dsu.get(label1).nodes.clone();
        for node in all_nodes {
            let key = (self.ref_dsu.get_repr(node), self.dsu.get(node).ref_depth);
            if let Some(label) = self.type_dsu.get(node).ref_map.get(&key) {
                if self.dsu.merge(*label, node) {
                    self.queue.push_back(node);
                }
            } else {
                self.type_dsu.get(node).ref_map.insert(key, node);
            }
        }
        Ok(())
    }

    pub fn hint_is(&mut self, label: IRTypeLabel, typ: IRPrimitiveType) -> CompilerResult<()> {
        self.set_type(label, IRType::Primitive(typ))?;
        self.set_ref(label, 0)?;

        self.run_queue()?;

        Ok(())
    }

    pub fn hint_equal(&mut self, label1: IRTypeLabel, label2: IRTypeLabel) -> CompilerResult<()> {
        self.merge(label1, label2)?;

        self.queue.push_back(label2);
        self.queue.push_back(label1);

        self.run_queue()?;
        Ok(())
    }

    pub fn hint_is_ref(&mut self, phys_label: IRTypeLabel, ref_label: IRTypeLabel) -> CompilerResult<()> {
        self.merge_type(phys_label, ref_label)?;

        self.merge_ref(phys_label, ref_label, 1)?;

        self.queue.push_back(phys_label);
        self.queue.push_back(ref_label);

        self.run_queue()?;
        Ok(())
    }

    pub fn hint_struct(&mut self, res_label: IRTypeLabel, struct_label: IRStructLabel, fields: Vec<IRTypeLabel>) -> CompilerResult<()> {
        for field_type in &fields {
            self.dsu.get(*field_type).neighbours.push(Conn::Struct(res_label, struct_label, fields.clone()));
            self.queue.push_back(*field_type);
        }
        self.dsu.get(res_label).neighbours.push(Conn::Struct(res_label, struct_label, fields));
        self.queue.push_back(res_label);
        self.set_ref(res_label, 0)?;

        self.run_queue()?;
        Ok(())
    }

    pub fn hint_is_field(&mut self, res_label: IRTypeLabel, struct_label: IRTypeLabel, field_label: IRFieldLabel) -> CompilerResult<()> {
        self.dsu.get(struct_label).neighbours.push(Conn::IsField(res_label, field_label));
        self.queue.push_back(struct_label);
        self.queue.push_back(res_label);
        self.set_ref(struct_label, 0)?;

        self.run_queue()?;
        Ok(())
    }

    pub fn hint_autoref(&mut self, label1: IRTypeLabel, label2: IRTypeLabel) -> CompilerResult<()> {
        self.merge_type(label1, label2)?;

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
