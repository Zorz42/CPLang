use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::normalizer::default_operator_map::setup_operator_map;
use crate::compiler::normalizer::ir::{IRAutoRefLabel, IRFieldLabel, IROperator, IRPrimitiveType, IRStructLabel, IRType, IRTypeLabel, IR};
use std::collections::VecDeque;

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
        autoref_label: IRAutoRefLabel,
        label1: IRTypeLabel,
        label2: IRTypeLabel,
    },
    // arg1 is physical (not a reference)
    IsPhys(IRTypeLabel),
}

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
    assert!(ref_depth >= 0);
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

pub fn resolve_types(ir: &mut IR, num_types: usize, type_positions: Vec<FilePosition>, type_hints: Vec<IRTypeHint>) -> CompilerResult<()> {
    // known types are without references and known refs are reference depths
    let mut known_types = vec![None; num_types];
    let mut known_refs = vec![None; num_types];
    let mut type_nodes = Vec::new();
    let mut auto_ref_pairs = vec![(0, 0); ir.autorefs.len()];
    for _ in 0..num_types {
        type_nodes.push(Vec::new());
    }
    let mut ref_nodes = Vec::new();
    for _ in 0..num_types {
        ref_nodes.push(Vec::new());
    }

    let operator_map = setup_operator_map();

    let try_set_type = |label: IRTypeLabel, typ: IRType, known_types: &mut Vec<Option<IRType>>, queue: &mut VecDeque<IRTypeLabel>| {
        if known_types[label].is_some() && known_types[label].as_ref() != Some(&typ) {
            return Err(CompilerError {
                message: format!("This cannot be of type {:?} and {:?} at the same time", known_types[label].as_ref().unwrap(), typ),
                position: Some(type_positions[label].clone()),
            });
        }
        if known_types[label].is_none() {
            known_types[label] = Some(typ);
            queue.push_back(label);
        }
        Ok(())
    };

    let try_set_ref = |label: IRTypeLabel, ref_val: i32, known_refs: &mut Vec<Option<i32>>, queue: &mut VecDeque<IRTypeLabel>| {
        if known_refs[label].is_some() && known_refs[label] != Some(ref_val) {
            return Err(CompilerError {
                message: format!("This cannot be {}-time reference and {}-time reference at the same time", known_refs[label].as_ref().unwrap(), ref_val),
                position: Some(type_positions[label].clone()),
            });
        }
        if known_refs[label].is_none() {
            known_refs[label] = Some(ref_val);
            queue.push_back(label);
        }
        Ok(())
    };

    let mut queue = VecDeque::new();
    for hint in type_hints {
        match hint {
            IRTypeHint::Is { label, typ } => {
                let ir_type = IRType::Primitive(typ);
                try_set_type(label, ir_type, &mut known_types, &mut queue)?;
                try_set_ref(label, 0, &mut known_refs, &mut queue)?;
            }
            IRTypeHint::Equal { label1, label2 } => {
                type_nodes[label1].push(Conn::Is(label2));
                type_nodes[label2].push(Conn::Is(label1));

                ref_nodes[label1].push((label2, 0));
                ref_nodes[label2].push((label1, 0));
            }
            IRTypeHint::Operator { label1, label2, operator, res_label } => {
                type_nodes[label1].push(Conn::Operator(res_label, operator, label2));
                type_nodes[label2].push(Conn::Operator(res_label, operator, label1));

                try_set_ref(label1, 0, &mut known_refs, &mut queue)?;
                try_set_ref(label2, 0, &mut known_refs, &mut queue)?;
            }
            IRTypeHint::IsRef { ref_label, phys_label } => {
                // ref_label = &phys_label
                type_nodes[phys_label].push(Conn::Is(ref_label));
                type_nodes[ref_label].push(Conn::Is(phys_label));

                ref_nodes[ref_label].push((phys_label, -1));
                ref_nodes[phys_label].push((ref_label, 1));
            }
            IRTypeHint::Struct { res_label, struct_label, fields } => {
                for field_type in &fields {
                    type_nodes[*field_type].push(Conn::Struct(res_label, struct_label, fields.clone()));
                }
                try_set_ref(res_label, 0, &mut known_refs, &mut queue)?;
            }
            IRTypeHint::IsField { res_label, struct_label, field_label } => {
                type_nodes[struct_label].push(Conn::IsField(res_label, field_label));
                try_set_ref(struct_label, 0, &mut known_refs, &mut queue)?;
            }
            IRTypeHint::AutoRef { autoref_label, label1, label2 } => {
                type_nodes[label1].push(Conn::Is(label2));
                type_nodes[label2].push(Conn::Is(label1));

                auto_ref_pairs[autoref_label] = (label1, label2);
            }
            IRTypeHint::IsPhys(label) => {
                try_set_ref(label, 0, &mut known_refs, &mut queue)?;
            }
        }
    }

    loop {
        let node = if let Some(node) = queue.pop_front() {
            node
        } else {
            break;
        };

        // loop for type deduction
        if let Some(node_type) = known_types[node].clone() {
            'neighbour_loop: for ne in &type_nodes[node] {
                match ne {
                    Conn::Is(ne) => {
                        try_set_type(*ne, node_type.clone(), &mut known_types, &mut queue)?;
                    }
                    Conn::Operator(ne, op, typ2) => {
                        if let Some(node_type2) = known_types[*typ2].clone() {
                            let ir_type = operator_map[&(node_type.clone(), *op, node_type2)].clone();
                            let (ir_type, ref_depth) = deref_type(ir_type);
                            try_set_type(*ne, ir_type, &mut known_types, &mut queue)?;
                            try_set_ref(*ne, ref_depth, &mut known_refs, &mut queue)?;
                        }
                    }
                    Conn::Struct(typ, structure, args) => {
                        // only deduce type when all arguments are known
                        let mut ir_args = Vec::new();
                        for arg in args {
                            if let Some(arg_type) = &known_types[*arg]
                                && let Some(ref_depth) = known_refs[*arg]
                            {
                                ir_args.push(ref_type(arg_type.clone(), ref_depth));
                            } else {
                                continue 'neighbour_loop;
                            }
                        }

                        let ir_type = IRType::Struct(*structure, ir_args);
                        try_set_type(*typ, ir_type, &mut known_types, &mut queue)?;
                    }
                    Conn::IsField(typ, field) => {
                        let (struct_label, struct_args) = match &known_types[node] {
                            Some(IRType::Struct(label, args)) => (*label, args),
                            _ => panic!(),
                        };
                        let curr_struct = &ir.structs[struct_label];
                        let mut field_idx = 0;
                        for (idx, curr_field) in curr_struct.fields.iter().enumerate() {
                            if curr_field == field {
                                field_idx = idx;
                            }
                        }

                        let ir_type = struct_args[field_idx].clone();
                        let (ir_type, ref_depth) = deref_type(ir_type);

                        try_set_type(*typ, ir_type, &mut known_types, &mut queue)?;
                        try_set_ref(*typ, ref_depth, &mut known_refs, &mut queue)?;
                    }
                }
            }
        }

        // loop for ref deduction
        if let Some(node_ref) = known_refs[node] {
            for (ne, delta) in &ref_nodes[node] {
                try_set_ref(*ne, node_ref + *delta, &mut known_refs, &mut queue)?;
            }
        }
    }

    for (i, (type1, type2)) in auto_ref_pairs.into_iter().enumerate() {
        ir.autorefs[i] = known_refs[type1].unwrap() - known_refs[type2].unwrap();
    }

    println!("{:?}", known_types);
    println!("{:?}", known_refs);
    for (typ, ref_depth) in known_types.into_iter().zip(known_refs) {
        ir.types.push(ref_type(typ.unwrap(), ref_depth.unwrap()));
    }

    Ok(())
}
