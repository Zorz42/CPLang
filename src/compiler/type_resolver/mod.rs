use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::normalizer::ir::{IRAutoRefLabel, IRFieldLabel, IROperator, IRPrimitiveType, IRStructLabel, IRType, IRTypeLabel, IR};
use crate::compiler::type_resolver::default_operator_map::setup_operator_map;
use crate::compiler::type_resolver::dsu::Dsu;
use std::collections::{HashMap, HashSet, VecDeque};
use std::mem::swap;
use std::ops::Add;

pub mod default_operator_map;
pub mod dsu;

#[derive(Clone)]
enum Conn {
    // node + arg1 = arg3 (+ is arg2)
    Operator(IRTypeLabel, IROperator, IRTypeLabel),

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

#[derive(Clone)]
pub struct TypeResolver {
    type_positions: Vec<FilePosition>,
    operator_map: HashMap<(IRType, IROperator, IRType), IRType>,
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
}

impl TypeResolver {
    pub fn new() -> Self {
        let mut res = Self {
            type_positions: Vec::new(),
            operator_map: setup_operator_map(),
            queue: VecDeque::new(),
            auto_ref_pairs: Vec::new(),
            dsu: Dsu::new(),
            type_dsu: Dsu::new(),
            ref_dsu: Dsu::new(),
            fixed_ref_component: 0,
            type_map: HashMap::new(),
        };
        let fixed = res.new_type_label(FilePosition::unknown());
        res.fixed_ref_component = fixed;
        res.type_dsu.get(res.fixed_ref_component).typ = Some(IRType::Primitive(IRPrimitiveType::Void));
        res
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

    fn get_ir_type(&mut self, label: IRTypeLabel) -> Option<IRType> {
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
                position: Some(self.type_positions[label].clone()),
            });
        }
        if self.type_dsu.get(label).typ.is_none() {
            if let Some(label2) = self.type_map.get(&typ) {
                self.merge_type(label, *label2)?;
            } else {
                self.type_map.insert(typ.clone(), label);
                self.type_dsu.get(label).typ = Some(typ);
                self.queue.push_back(label);
            }
        }
        Ok(())
    }

    fn set_ref(&mut self, label: IRTypeLabel, ref_val: i32) -> CompilerResult<()> {
        self.merge_ref(self.fixed_ref_component, label, ref_val)
    }

    fn run_queue(&mut self, ir: &IR) -> CompilerResult<()> {
        while let Some(node) = self.queue.pop_front() {
            // loop for type deduction
            if let Some(node_type) = self.type_dsu.get(node).typ.clone() {
                'neighbour_loop: for ne in self.dsu.get(node).neighbours.clone() {
                    match ne {
                        Conn::Operator(ne, op, typ2) => {
                            if let Some(node_type2) = self.type_dsu.get(typ2).typ.clone() {
                                let Some(ir_type) = self.operator_map.get(&(node_type.clone(), op, node_type2.clone())).cloned() else {
                                    return Err(CompilerError {
                                        message: format!("Operator {op:?} is not defined for {node_type2:?} and {node_type:?}"),
                                        position: Some(self.type_positions[ne].clone()),
                                    });
                                };
                                let (ir_type, ref_depth) = deref_type(ir_type);
                                self.set_type(ne, ir_type)?;
                                self.set_ref(ne, ref_depth)?;
                            }
                        }
                        Conn::Struct(typ, structure, args) => {
                            if typ == node {
                                let IRType::Struct(_, args2) = node_type.clone() else {
                                    return Err(CompilerError {
                                        message: format!("This cannot be a struct and {node_type:?} at the same time."),
                                        position: Some(self.type_positions[node].clone()),
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
                                if let Some(typ) = self.get_ir_type(arg) {
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
                                        position: Some(self.type_positions[node].clone()),
                                    });
                                }
                            };
                            let curr_struct = &ir.structs[struct_label];
                            let mut field_idx = 0;
                            for (idx, curr_field) in curr_struct.fields.iter().enumerate() {
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
                    for (arg, field_label) in args.iter().zip(ir.structs[*structure].fields.iter()) {
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
                position: Some(self.type_positions[label1].clone()),
            });
        }

        self.type_dsu.get(label1).typ = typ1;
        self.type_dsu.get(label2).typ = typ2;

        let mut ref_map = HashMap::new();
        swap(&mut ref_map, &mut self.type_dsu.get(label1).ref_map);
        for (key, label) in ref_map {
            if let Some(other_label) = self.type_dsu.get(label2).ref_map.get(&key).copied() {
                self.dsu.merge(label, other_label);
                self.queue.push_back(label);
            } else {
                self.type_dsu.get(label2).ref_map.insert(key, label);
            }
        }

        if self.type_dsu.merge(label1, label2) {
            self.queue.push_back(label1);
            self.queue.push_back(label2);
        }

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
                    position: Some(self.type_positions[label1].clone()),
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

        self.ref_dsu.merge(label1, label2);

        all_nodes = self.ref_dsu.get(label1).nodes.clone();
        for node in all_nodes {
            let key = (self.ref_dsu.get_repr(node), self.dsu.get(node).ref_depth);
            if let Some(label) = self.type_dsu.get(node).ref_map.get(&key) {
                self.dsu.merge(*label, node);
                self.queue.push_back(node);
            } else {
                self.type_dsu.get(node).ref_map.insert(key, node);
            }
        }
        Ok(())
    }

    pub fn hint_is(&mut self, ir: &IR, label: IRTypeLabel, typ: IRPrimitiveType) -> CompilerResult<()> {
        let ir_type = IRType::Primitive(typ);
        self.set_type(label, ir_type)?;
        self.set_ref(label, 0)?;

        self.run_queue(ir)?;
        Ok(())
    }

    pub fn hint_equal(&mut self, ir: &IR, label1: IRTypeLabel, label2: IRTypeLabel) -> CompilerResult<()> {
        self.merge(label1, label2)?;

        self.queue.push_back(label2);
        self.queue.push_back(label1);

        self.run_queue(ir)?;
        Ok(())
    }

    pub fn hint_operator(&mut self, ir: &IR, label1: IRTypeLabel, label2: IRTypeLabel, operator: IROperator, res_label: IRTypeLabel) -> CompilerResult<()> {
        self.dsu.get(label1).neighbours.push(Conn::Operator(res_label, operator, label2));
        self.dsu.get(label2).neighbours.push(Conn::Operator(res_label, operator, label1));

        self.set_ref(label1, 0)?;
        self.set_ref(label2, 0)?;

        self.queue.push_back(label1);
        self.queue.push_back(label2);

        self.run_queue(ir)?;
        Ok(())
    }

    pub fn hint_is_ref(&mut self, ir: &IR, phys_label: IRTypeLabel, ref_label: IRTypeLabel) -> CompilerResult<()> {
        self.merge_type(phys_label, ref_label)?;

        self.merge_ref(phys_label, ref_label, 1)?;

        self.queue.push_back(phys_label);
        self.queue.push_back(ref_label);

        self.run_queue(ir)?;
        Ok(())
    }

    pub fn hint_struct(&mut self, ir: &IR, res_label: IRTypeLabel, struct_label: IRStructLabel, fields: Vec<IRTypeLabel>) -> CompilerResult<()> {
        for field_type in &fields {
            self.dsu
                .get(*field_type)
                .neighbours
                .push(Conn::Struct(res_label, struct_label, fields.clone()));
            self.queue.push_back(*field_type);
        }
        self.dsu.get(res_label).neighbours.push(Conn::Struct(res_label, struct_label, fields));
        self.queue.push_back(res_label);
        self.set_ref(res_label, 0)?;

        self.run_queue(ir)?;
        Ok(())
    }

    pub fn hint_is_field(&mut self, ir: &IR, res_label: IRTypeLabel, struct_label: IRTypeLabel, field_label: IRFieldLabel) -> CompilerResult<()> {
        self.dsu.get(struct_label).neighbours.push(Conn::IsField(res_label, field_label));
        self.queue.push_back(struct_label);
        self.queue.push_back(res_label);
        self.set_ref(struct_label, 0)?;

        self.run_queue(ir)?;
        Ok(())
    }

    pub fn hint_autoref(&mut self, ir: &IR, label1: IRTypeLabel, label2: IRTypeLabel) -> CompilerResult<()> {
        self.merge_type(label1, label2)?;

        self.run_queue(ir)?;
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
            let Some(typ) = self.get_ir_type(type_label) else {
                return Err(CompilerError {
                    message: "Could not deduce this expression's type".to_string(),
                    position: Some(self.type_positions[type_label].clone()),
                });
            };

            let ref_depth = self.dsu.get(type_label).ref_depth;

            if ref_depth < 0 {
                let pos = self.type_positions[type_label].clone();
                return Err(CompilerError {
                    message: "This has type of a dereferenced non-reference".to_string(),
                    position: Some(pos),
                });
            }

            types.insert(type_label, typ);
        }

        Ok((types, autorefs))
    }

    pub fn are_equal(&mut self, label1: IRTypeLabel, label2: IRTypeLabel) -> bool {
        self.dsu.get_repr(label1) == self.dsu.get_repr(label2)
    }
}
