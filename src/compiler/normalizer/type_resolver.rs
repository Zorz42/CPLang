use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::normalizer::default_operator_map::setup_operator_map;
use crate::compiler::normalizer::ir::{IRAutoRefLabel, IRFieldLabel, IROperator, IRPrimitiveType, IRStructLabel, IRType, IRTypeLabel, IR};
use std::collections::{HashMap, VecDeque};

pub enum IRTypeHint {
    Is {
        label: IRTypeLabel,
        typ: IRPrimitiveType,
    },
    Equal {
        label1: IRTypeLabel,
        label2: IRTypeLabel,
    },
    // res = label1 + label2, + is operator
    Operator {
        label1: IRTypeLabel,
        label2: IRTypeLabel,
        operator: IROperator,
        res_label: IRTypeLabel,
    },
    // ref = &phys
    IsRef {
        ref_label: IRTypeLabel,
        phys_label: IRTypeLabel,
    },
    // res = struct { fields[0], ..., fields[n-1] }
    Struct {
        res_label: IRTypeLabel,
        struct_label: IRStructLabel,
        fields: Vec<IRTypeLabel>,
    },
    // res = struct.field
    IsField {
        res_label: IRTypeLabel,
        struct_label: IRTypeLabel,
        field_label: IRFieldLabel,
    },
    // label1 = &&...&&&label2 or ::...:::label2
    // automatically reference/dereference. both are type typ
    AutoRef {
        label1: IRTypeLabel,
        label2: IRTypeLabel,
    },
    // arg1 is physical (not a reference)
    IsPhys(IRTypeLabel),
}

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

fn ref_type(mut typ: IRType, ref_depth: i32) -> IRType {
    for _ in 0..ref_depth {
        typ = IRType::Reference(Box::new(typ));
    }
    typ
}

fn deref_type(mut ir_type: IRType) -> (IRType, i32) {
    let mut ref_depth = 0;

    while let IRType::Reference(inner) = ir_type {
        ir_type = *inner;
        ref_depth += 1;
    }

    (ir_type, ref_depth)
}

pub struct TypeResolver {
    type_positions: Vec<FilePosition>,
    operator_map: HashMap<(IRType, IROperator, IRType), IRType>,
    known_types: Vec<Option<IRType>>,
    known_refs: Vec<Option<i32>>,
    type_nodes: Vec<Vec<Conn>>,
    ref_nodes: Vec<Vec<(IRTypeLabel, i32)>>,
    queue: VecDeque<IRTypeLabel>,
    auto_ref_pairs: Vec<(IRTypeLabel, IRTypeLabel)>,
}

impl TypeResolver {
    pub fn new() -> Self {
        Self {
            type_positions: Vec::new(),
            operator_map: setup_operator_map(),
            known_types: Vec::new(),
            known_refs: Vec::new(),
            type_nodes: Vec::new(),
            ref_nodes: Vec::new(),
            queue: VecDeque::new(),
            auto_ref_pairs: Vec::new(),
        }
    }

    pub fn new_type_label(&mut self, pos: FilePosition) -> IRTypeLabel {
        let res = self.known_types.len() as IRTypeLabel;
        self.known_types.push(None);
        self.known_refs.push(None);
        self.type_nodes.push(Vec::new());
        self.ref_nodes.push(Vec::new());
        self.type_positions.push(pos);
        res
    }

    pub fn new_autoref_label(&mut self, label1: IRTypeLabel, label2: IRTypeLabel) -> IRAutoRefLabel {
        let res = self.auto_ref_pairs.len() as IRAutoRefLabel;
        self.auto_ref_pairs.push((label1, label2));
        res
    }

    fn try_set_type(&mut self, label: IRTypeLabel, typ: IRType) -> CompilerResult<()> {
        if self.known_types[label].is_some() && self.known_types[label].as_ref() != Some(&typ) {
            return Err(CompilerError {
                message: format!("This cannot be of type {:?} and {:?} at the same time", self.known_types[label].as_ref().unwrap(), typ),
                position: Some(self.type_positions[label].clone()),
            });
        }
        if self.known_types[label].is_none() {
            self.known_types[label] = Some(typ);
            self.queue.push_back(label);
        }
        Ok(())
    }

    fn try_set_ref(&mut self, label: IRTypeLabel, ref_val: i32) -> CompilerResult<()> {
        if self.known_refs[label].is_some() && self.known_refs[label] != Some(ref_val) {
            return Err(CompilerError {
                message: format!("This cannot be {}-time reference and {}-time reference at the same time", self.known_refs[label].as_ref().unwrap(), ref_val),
                position: Some(self.type_positions[label].clone()),
            });
        }
        if self.known_refs[label].is_none() {
            self.known_refs[label] = Some(ref_val);
            self.queue.push_back(label);
        }
        Ok(())
    }

    fn run_queue(&mut self, ir: &mut IR) -> CompilerResult<()> {
        while let Some(node) = self.queue.pop_front() {
            // loop for type deduction
            if let Some(node_type) = self.known_types[node].clone() {
                'neighbour_loop: for ne in self.type_nodes[node].clone() {
                    match ne {
                        Conn::Is(ne) => {
                            self.try_set_type(ne, node_type.clone())?;
                        }
                        Conn::Operator(ne, op, typ2) => {
                            if let Some(node_type2) = self.known_types[typ2].clone() {
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
                            // only deduce type when all arguments are known
                            let mut ir_args = Vec::new();
                            for arg in args {
                                if let Some(arg_type) = &self.known_types[arg]
                                    && let Some(ref_depth) = self.known_refs[arg]
                                {
                                    ir_args.push(ref_type(arg_type.clone(), ref_depth));
                                } else {
                                    continue 'neighbour_loop;
                                }
                            }

                            let ir_type = IRType::Struct(structure, ir_args);
                            self.try_set_type(typ, ir_type)?;
                        }
                        Conn::IsField(typ, field) => {
                            let (struct_label, struct_args) = match &self.known_types[node] {
                                Some(IRType::Struct(label, args)) => (*label, args),
                                _ => return Err(CompilerError {
                                    message: format!("Cannot access field since this is not a struct but has type {:?}", self.known_types[node].as_ref().unwrap()),
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

            // loop for ref deduction
            if let Some(node_ref) = self.known_refs[node] {
                for (ne, delta) in self.ref_nodes[node].clone() {
                    self.try_set_ref(ne, node_ref + delta)?;
                }
            }
        }

        Ok(())
    }

    pub fn hint(&mut self, ir: &mut IR, hint: IRTypeHint) -> CompilerResult<()> {
        match hint {
            IRTypeHint::Is { label, typ } => {
                let ir_type = IRType::Primitive(typ);
                self.try_set_type(label, ir_type)?;
                self.try_set_ref(label, 0)?;
            }
            IRTypeHint::Equal { label1, label2 } => {
                self.type_nodes[label1].push(Conn::Is(label2));
                self.type_nodes[label2].push(Conn::Is(label1));

                self.ref_nodes[label1].push((label2, 0));
                self.ref_nodes[label2].push((label1, 0));

                self.queue.push_back(label2);
                self.queue.push_back(label1);
            }
            IRTypeHint::Operator { label1, label2, operator, res_label } => {
                self.type_nodes[label1].push(Conn::Operator(res_label, operator, label2));
                self.type_nodes[label2].push(Conn::Operator(res_label, operator, label1));

                self.try_set_ref(label1, 0)?;
                self.try_set_ref(label2, 0)?;

                self.queue.push_back(label1);
                self.queue.push_back(label2);
            }
            IRTypeHint::IsRef { ref_label, phys_label } => {
                // ref_label = &phys_label
                self.type_nodes[phys_label].push(Conn::Is(ref_label));
                self.type_nodes[ref_label].push(Conn::Is(phys_label));

                self.ref_nodes[ref_label].push((phys_label, -1));
                self.ref_nodes[phys_label].push((ref_label, 1));

                self.queue.push_back(phys_label);
                self.queue.push_back(ref_label);
            }
            IRTypeHint::Struct { res_label, struct_label, fields } => {
                for field_type in &fields {
                    self.type_nodes[*field_type].push(Conn::Struct(res_label, struct_label, fields.clone()));
                    self.queue.push_back(*field_type);
                }
                self.try_set_ref(res_label, 0)?;
            }
            IRTypeHint::IsField { res_label, struct_label, field_label } => {
                self.type_nodes[struct_label].push(Conn::IsField(res_label, field_label));
                self.try_set_ref(struct_label, 0)?;
                self.queue.push_back(struct_label);
            }
            IRTypeHint::AutoRef { label1, label2 } => {
                self.type_nodes[label1].push(Conn::Is(label2));
                self.type_nodes[label2].push(Conn::Is(label1));

                self.queue.push_back(label1);
                self.queue.push_back(label2);
            }
            IRTypeHint::IsPhys(label) => {
                self.try_set_ref(label, 0)?;
            }
        }

        self.run_queue(ir)?;

        Ok(())
    }

    pub fn gather_types(self) -> CompilerResult<(Vec<IRType>, Vec<i32>)> {
        let mut types = Vec::new();
        let mut autorefs = vec![0; self.auto_ref_pairs.len()];

        for (i, (type1, type2)) in self.auto_ref_pairs.into_iter().enumerate() {
            autorefs[i] = self.known_refs[type1].unwrap() - self.known_refs[type2].unwrap();
        }

        println!("{:?}", self.known_types);
        println!("{:?}", self.known_refs);
        println!("{:?}", self.type_positions);
        for ((typ, ref_depth), pos) in self.known_types.into_iter().zip(self.known_refs).zip(self.type_positions) {
            if ref_depth.unwrap() < 0 {
                return Err(CompilerError {
                    message: "This has type of a dereferenced non-reference".to_string(),
                    position: Some(pos),
                });
            }
            types.push(ref_type(typ.unwrap(), ref_depth.unwrap()));
        }

        Ok((types, autorefs))
    }
}