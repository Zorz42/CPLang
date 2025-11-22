use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::normalizer::default_operator_map::setup_operator_map;
use crate::compiler::normalizer::dsu::Dsu;
use crate::compiler::normalizer::ir::{IRAutoRefLabel, IRFieldLabel, IROperator, IRPrimitiveType, IRStructLabel, IRType, IRTypeLabel, IR};
use std::collections::{HashMap, VecDeque};
use std::ops::Add;

#[derive(Clone)]
enum Conn {
    // node = arg1
    Is(IRTypeLabel),

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

#[derive(Default)]
pub struct Node {
    neighbours: Vec<Conn>,
    ref_neighbours: Vec<(IRTypeLabel, i32)>,
    typ: Option<IRType>,
    ref_depth: Option<i32>,
}

impl Add for Node {
    type Output = Node;

    fn add(mut self, rhs: Self) -> Self::Output {
        assert_eq!(self.typ, rhs.typ);
        assert_eq!(self.ref_depth, rhs.ref_depth);

        for i in rhs.neighbours {
            self.neighbours.push(i);
        }
        self
    }
}

impl Node {
    fn get_ir_type(&self) -> Option<IRType> {
        let mut typ = self.typ.clone()?;
        let dep = self.ref_depth?;
        if dep < 0 {
            return None;
        }

        for _ in 0..dep {
            typ = IRType::Reference(Box::new(typ))
        }
        Some(typ)
    }
}

pub struct TypeResolver {
    type_positions: Vec<FilePosition>,
    operator_map: HashMap<(IRType, IROperator, IRType), IRType>,
    queue: VecDeque<IRTypeLabel>,
    auto_ref_pairs: Vec<(IRTypeLabel, IRTypeLabel)>,
    types_dsu: Dsu<Node>,
}

impl TypeResolver {
    pub fn new() -> Self {
        Self {
            type_positions: Vec::new(),
            operator_map: setup_operator_map(),
            queue: VecDeque::new(),
            auto_ref_pairs: Vec::new(),
            types_dsu: Dsu::new(),
        }
    }

    pub fn new_type_label(&mut self, pos: FilePosition) -> IRTypeLabel {
        let res = self.types_dsu.len() as IRTypeLabel;
        self.types_dsu.add();
        self.type_positions.push(pos);
        res
    }

    pub fn new_autoref_label(&mut self, label1: IRTypeLabel, label2: IRTypeLabel) -> IRAutoRefLabel {
        let res = self.auto_ref_pairs.len() as IRAutoRefLabel;
        self.auto_ref_pairs.push((label1, label2));
        res
    }

    fn try_set_type(&mut self, label: IRTypeLabel, typ: IRType) -> CompilerResult<()> {
        if self.types_dsu.get(label).typ.is_some() && self.types_dsu.get(label).typ.as_ref() != Some(&typ) {
            return Err(CompilerError {
                message: format!("This cannot be of type {:?} and {:?} at the same time", self.types_dsu.get(label).typ.as_ref().unwrap(), typ),
                position: Some(self.type_positions[label].clone()),
            });
        }
        if self.types_dsu.get(label).typ.is_none() {
            self.types_dsu.get(label).typ = Some(typ);
            self.queue.push_back(label);
        }
        Ok(())
    }

    fn try_set_ref(&mut self, label: IRTypeLabel, ref_val: i32) -> CompilerResult<()> {
        if self.types_dsu.get(label).ref_depth.is_some() && self.types_dsu.get(label).ref_depth != Some(ref_val) {
            return Err(CompilerError {
                message: format!("This cannot be {}-time reference and {}-time reference at the same time", self.types_dsu.get(label).ref_depth.as_ref().unwrap(), ref_val),
                position: Some(self.type_positions[label].clone()),
            });
        }
        if self.types_dsu.get(label).ref_depth.is_none() {
            self.types_dsu.get(label).ref_depth = Some(ref_val);
            self.queue.push_back(label);
        }
        Ok(())
    }

    fn run_queue(&mut self, ir: &mut IR) -> CompilerResult<()> {
        while let Some(node) = self.queue.pop_front() {
            // loop for type deduction
            if let Some(node_type) = self.types_dsu.get(node).typ.clone() {
                'neighbour_loop: for ne in self.types_dsu.get(node).neighbours.clone() {
                    match ne {
                        Conn::Is(ne) => {
                            self.try_set_type(ne, node_type.clone())?;
                        }
                        Conn::Operator(ne, op, typ2) => {
                            if let Some(node_type2) = self.types_dsu.get(typ2).typ.clone() {
                                let ir_type = if let Some(x) = self.operator_map.get(&(node_type.clone(), op, node_type2.clone())).cloned() {
                                    x
                                } else {
                                    return Err(CompilerError {
                                        message: format!("Operator {:?} is not defined for {:?} and {:?}", op, node_type2, node_type),
                                        position: Some(self.type_positions[ne].clone()),
                                    })
                                };
                                let (ir_type, ref_depth) = deref_type(ir_type);
                                self.try_set_type(ne, ir_type)?;
                                self.try_set_ref(ne, ref_depth)?;
                            }
                        }
                        Conn::Struct(typ, structure, args) => {
                            if typ == node {
                                let args2 = match node_type.clone() {
                                    IRType::Struct(_, args) => args,
                                    _ => return Err(CompilerError {
                                        message: format!("This cannot be a struct and {node_type:?} at the same time."),
                                        position: Some(self.type_positions[node].clone()),
                                    })
                                };

                                for (typ_label, typ) in args.into_iter().zip(args2) {
                                    let (typ, ref_depth) = deref_type(typ);
                                    self.try_set_type(typ_label, typ)?;
                                    self.try_set_ref(typ_label, ref_depth)?;
                                }

                                continue 'neighbour_loop;
                            }

                            // only deduce type when all arguments are known
                            let mut ir_args = Vec::new();
                            for arg in args {
                                if let Some(typ) = self.types_dsu.get(arg).get_ir_type() {
                                    ir_args.push(typ);
                                } else {
                                    continue 'neighbour_loop;
                                }
                            }

                            let ir_type = IRType::Struct(structure, ir_args);
                            self.try_set_type(typ, ir_type)?;
                        }
                        Conn::IsField(typ, field) => {
                            let (struct_label, struct_args) = match &self.types_dsu.get(node).typ {
                                Some(IRType::Struct(label, args)) => (*label, args),
                                _ => return Err(CompilerError {
                                    message: format!("Cannot access field since this is not a struct but has type {:?}", self.types_dsu.get(node).typ.as_ref().unwrap()),
                                    position: Some(self.type_positions[node].clone()),
                                })
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

                            self.try_set_type(typ, ir_type)?;
                            self.try_set_ref(typ, ref_depth)?;
                        }
                    }
                }
            }


            if self.types_dsu.get(node).typ.is_none() {
                // if the type is struct, save its fields (if they exist).
                let mut struct_fields: Option<(IRStructLabel, Vec<IRTypeLabel>)> = None;

                for ne in self.types_dsu.get(node).neighbours.clone() {
                    if let Conn::Struct(typ, structure, args) = ne && self.are_equal(typ, node) {
                        if let Some((curr_structure, curr_args)) = &struct_fields {
                            if *curr_structure != structure {
                                panic!();
                            }

                            for (arg1, arg2) in args.iter().zip(curr_args.clone()) {
                                self.merge(*arg1, arg2)?;
                            }
                        } else {
                            struct_fields = Some((structure, args.clone()));
                        }
                    }
                }

                let mut fields = HashMap::new();

                if let Some((structure, args)) = struct_fields {
                    for (arg, field_label) in args.iter().zip(ir.structs[structure].fields.iter()) {
                        fields.insert(*field_label, *arg);
                    }
                }

                for ne in self.types_dsu.get(node).neighbours.clone() {
                    if let Conn::IsField(typ, field) = ne {
                        if let Some(typ2) = fields.get(&field) {
                            self.merge(typ, *typ2)?;
                        } else {
                            fields.insert(field, typ);
                        }
                    }
                }
            }


            // loop for ref deduction
            if let Some(node_ref) = self.types_dsu.get(node).ref_depth {
                for (ne, delta) in self.types_dsu.get(node).ref_neighbours.clone() {
                    self.try_set_ref(ne, node_ref + delta)?;
                }
            }
        }

        Ok(())
    }

    fn merge(&mut self, label1: IRTypeLabel, label2: IRTypeLabel) -> CompilerResult<()> {
        let mut typ1 = self.types_dsu.get(label1).typ.clone();
        let mut typ2 = self.types_dsu.get(label2).typ.clone();

        if typ1.is_none() {
            typ1 = typ2.clone();
        } else if typ2.is_none() {
            typ2 = typ1.clone();
        } else if typ1 != typ2 {
            return Err(CompilerError {
                message: format!("This expression cannot be {:?} and {:?} at the same time.", typ1.unwrap(), typ2.unwrap()),
                position: Some(self.type_positions[label1].clone()),
            })
        }

        self.types_dsu.get(label1).typ = typ1;
        self.types_dsu.get(label2).typ = typ2;

        let mut ref1 = self.types_dsu.get(label1).ref_depth;
        let mut ref2 = self.types_dsu.get(label2).ref_depth;

        if ref1.is_none() {
            ref1 = ref2;
        } else if ref2.is_none() {
            ref2 = ref1;
        } else if ref1 != ref2 {
            return Err(CompilerError {
                message: format!("This expression cannot be {:?}-time and {:?}-time reference at the same time.", ref1.unwrap(), ref1.unwrap()),
                position: Some(self.type_positions[label1].clone()),
            })
        }

        self.types_dsu.get(label1).ref_depth = ref1;
        self.types_dsu.get(label2).ref_depth = ref2;

        if self.types_dsu.merge(label1, label2) {
            self.queue.push_back(label1);
        }

        Ok(())
    }

    pub fn hint_is(&mut self, ir: &mut IR, label: IRTypeLabel, typ: IRPrimitiveType) -> CompilerResult<()> {
        let ir_type = IRType::Primitive(typ);
        self.try_set_type(label, ir_type)?;
        self.try_set_ref(label, 0)?;

        self.run_queue(ir)?;
        Ok(())
    }

    pub fn hint_equal(&mut self, ir: &mut IR, label1: IRTypeLabel, label2: IRTypeLabel) -> CompilerResult<()> {
        self.merge(label1, label2)?;

        self.queue.push_back(label2);
        self.queue.push_back(label1);

        self.run_queue(ir)?;
        Ok(())
    }

    pub fn hint_operator(&mut self, ir: &mut IR, label1: IRTypeLabel, label2: IRTypeLabel, operator: IROperator, res_label: IRTypeLabel) -> CompilerResult<()> {
        self.types_dsu.get(label1).neighbours.push(Conn::Operator(res_label, operator, label2));
        self.types_dsu.get(label2).neighbours.push(Conn::Operator(res_label, operator, label1));

        self.try_set_ref(label1, 0)?;
        self.try_set_ref(label2, 0)?;

        self.queue.push_back(label1);
        self.queue.push_back(label2);

        self.run_queue(ir)?;
        Ok(())
    }

    pub fn hint_is_ref(&mut self, ir: &mut IR, phys_label: IRTypeLabel, ref_label: IRTypeLabel) -> CompilerResult<()> {
        self.types_dsu.get(phys_label).neighbours.push(Conn::Is(ref_label));
        self.types_dsu.get(ref_label).neighbours.push(Conn::Is(phys_label));

        self.types_dsu.get(ref_label).ref_neighbours.push((phys_label, -1));
        self.types_dsu.get(phys_label).ref_neighbours.push((ref_label, 1));

        self.queue.push_back(phys_label);
        self.queue.push_back(ref_label);

        self.run_queue(ir)?;
        Ok(())
    }

    pub fn hint_struct(&mut self, ir: &mut IR, res_label: IRTypeLabel, struct_label: IRStructLabel, fields: Vec<IRTypeLabel>) -> CompilerResult<()> {
        for field_type in &fields {
            self.types_dsu.get(*field_type).neighbours.push(Conn::Struct(res_label, struct_label, fields.clone()));
            self.queue.push_back(*field_type);
        }
        self.types_dsu.get(res_label).neighbours.push(Conn::Struct(res_label, struct_label, fields.clone()));
        self.queue.push_back(res_label);
        self.try_set_ref(res_label, 0)?;

        self.run_queue(ir)?;
        Ok(())
    }

    pub fn hint_is_field(&mut self, ir: &mut IR, res_label: IRTypeLabel, struct_label: IRTypeLabel, field_label: IRFieldLabel) -> CompilerResult<()> {
        self.types_dsu.get(struct_label).neighbours.push(Conn::IsField(res_label, field_label));
        self.try_set_ref(struct_label, 0)?;
        self.queue.push_back(struct_label);
        self.queue.push_back(res_label);

        self.run_queue(ir)?;
        Ok(())
    }

    pub fn hint_autoref(&mut self, ir: &mut IR, label1: IRTypeLabel, label2: IRTypeLabel) -> CompilerResult<()> {
        self.types_dsu.get(label1).neighbours.push(Conn::Is(label2));
        self.types_dsu.get(label2).neighbours.push(Conn::Is(label1));

        self.queue.push_back(label1);
        self.queue.push_back(label2);

        self.run_queue(ir)?;
        Ok(())
    }

    pub fn gather_types(mut self) -> CompilerResult<(Vec<IRType>, Vec<i32>)> {
        let mut types = Vec::new();
        let mut autorefs = vec![0; self.auto_ref_pairs.len()];

        for (i, (type1, type2)) in self.auto_ref_pairs.into_iter().enumerate() {
            autorefs[i] = self.types_dsu.get(type1).ref_depth.unwrap() - self.types_dsu.get(type2).ref_depth.unwrap();
        }

        for i in 0..self.types_dsu.len() {
            let typ = if let Some(typ) = self.types_dsu.get(i).get_ir_type() {
                typ
            } else {
                return Err(CompilerError {
                    message: "Could not deduce this expression's type".to_string(),
                    position: Some(self.type_positions[i].clone()),
                });
            };

            let ref_depth = self.types_dsu.get(i).ref_depth.unwrap();

            if ref_depth < 0 {
                let pos = self.type_positions[i].clone();
                return Err(CompilerError {
                    message: "This has type of a dereferenced non-reference".to_string(),
                    position: Some(pos),
                });
            }

            types.push(typ);
        }

        Ok((types, autorefs))
    }

    // false positive
    #[allow(clippy::clone_on_copy)]
    pub fn are_equal(&mut self, label1: IRTypeLabel, label2: IRTypeLabel) -> bool {
        if self.types_dsu.get_repr(label1) == self.types_dsu.get_repr(label2) {
            return true;
        }

        if self.types_dsu.get(label1).typ.is_some() && self.types_dsu.get(label2).ref_depth.is_some() &&
            self.types_dsu.get(label1).typ.clone() == self.types_dsu.get(label2).typ &&
            self.types_dsu.get(label1).ref_depth.clone() == self.types_dsu.get(label2).ref_depth {
            return true;
        }

        false
    }
}