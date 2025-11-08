use std::collections::{HashMap, HashSet};
use crate::compiler::normalizer::ir::{IRAutoRefLabel, IRFieldLabel, IROperator, IRPrimitiveType, IRStructLabel, IRType, IRTypeLabel, IR};

pub enum IRTypeHint {
    Is(IRTypeLabel, IRPrimitiveType),
    Equal(IRTypeLabel, IRTypeLabel),
    // arg1 = arg2 + arg4, + is arg3
    Operator(IRTypeLabel, IRTypeLabel, IROperator, IRTypeLabel),
    // arg1 = &arg2
    IsRef(IRTypeLabel, IRTypeLabel),
    // arg1 = arg2 { arg3[0], ..., arg3[n] }
    Struct(IRTypeLabel, IRStructLabel, Vec<IRTypeLabel>),
    // arg1 = arg2.arg3
    IsField(IRTypeLabel, IRTypeLabel, IRFieldLabel),
    // &&...&&&arg1 = &&...&&&arg2
    // automatically reference/dereference arg1 to arg0
    AutoRef(IRAutoRefLabel, IRTypeLabel, IRTypeLabel),
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

fn setup_operator_map() -> HashMap<(IRType, IROperator, IRType), IRType> {
    let mut operator_map = HashMap::new();

    operator_map.insert((IRType::Primitive(IRPrimitiveType::I32), IROperator::Plus, IRType::Primitive(IRPrimitiveType::I32)), IRType::Primitive(IRPrimitiveType::I32));
    operator_map.insert((IRType::Primitive(IRPrimitiveType::I32), IROperator::Minus, IRType::Primitive(IRPrimitiveType::I32)), IRType::Primitive(IRPrimitiveType::I32));
    operator_map.insert((IRType::Primitive(IRPrimitiveType::I32), IROperator::Mul, IRType::Primitive(IRPrimitiveType::I32)), IRType::Primitive(IRPrimitiveType::I32));
    operator_map.insert((IRType::Primitive(IRPrimitiveType::I32), IROperator::Div, IRType::Primitive(IRPrimitiveType::I32)), IRType::Primitive(IRPrimitiveType::I32));

    operator_map.insert((IRType::Primitive(IRPrimitiveType::I32), IROperator::Equals, IRType::Primitive(IRPrimitiveType::I32)), IRType::Primitive(IRPrimitiveType::Bool));
    operator_map.insert((IRType::Primitive(IRPrimitiveType::I32), IROperator::NotEquals, IRType::Primitive(IRPrimitiveType::I32)), IRType::Primitive(IRPrimitiveType::Bool));
    operator_map.insert((IRType::Primitive(IRPrimitiveType::I32), IROperator::Greater, IRType::Primitive(IRPrimitiveType::I32)), IRType::Primitive(IRPrimitiveType::Bool));
    operator_map.insert((IRType::Primitive(IRPrimitiveType::I32), IROperator::GreaterOrEq, IRType::Primitive(IRPrimitiveType::I32)), IRType::Primitive(IRPrimitiveType::Bool));
    operator_map.insert((IRType::Primitive(IRPrimitiveType::I32), IROperator::Lesser, IRType::Primitive(IRPrimitiveType::I32)), IRType::Primitive(IRPrimitiveType::Bool));
    operator_map.insert((IRType::Primitive(IRPrimitiveType::I32), IROperator::LesserOrEq, IRType::Primitive(IRPrimitiveType::I32)), IRType::Primitive(IRPrimitiveType::Bool));

    operator_map
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

pub fn resolve_types(ir: &mut IR, num_types: usize, type_hints: Vec<IRTypeHint>) {
    // known types are without references and known refs are reference depths
    let mut known_types = vec![None; num_types];
    let mut known_refs = vec![None; num_types];
    let mut type_nodes = Vec::new();
    for _ in 0..num_types {
        type_nodes.push(Vec::new());
    }
    let mut ref_nodes = Vec::new();
    for _ in 0..num_types {
        ref_nodes.push(Vec::new());
    }

    let operator_map = setup_operator_map();

    let try_set_type = |
        label: IRTypeLabel,
        typ: IRType,
        known_types: &mut Vec<Option<IRType>>,
        queue: &mut Vec<IRTypeLabel>
    | {
        if known_types[label].is_some() && known_types[label].as_ref() != Some(&typ) {
            panic!();
        }
        if known_types[label].is_none() {
            known_types[label] = Some(typ);
            queue.push(label);
        }
    };

    let try_set_ref = |
        label: IRTypeLabel,
        ref_val: i32,
        known_refs: &mut Vec<Option<i32>>,
        queue: &mut Vec<IRTypeLabel>
    | {
        if known_refs[label].is_some() && known_refs[label] != Some(ref_val) {
            panic!();
        }
        if known_refs[label].is_none() {
            known_refs[label] = Some(ref_val);
            queue.push(label);
        }
    };

    let mut queue = Vec::new();
    for hint in type_hints {
        match hint {
            IRTypeHint::Is(label, typ) => {
                let ir_type = IRType::Primitive(typ);
                try_set_type(label, ir_type, &mut known_types, &mut queue);
                try_set_ref(label, 0, &mut known_refs, &mut queue);
            }
            IRTypeHint::Equal(label1, label2) => {
                type_nodes[label1].push(Conn::Is(label2));
                type_nodes[label2].push(Conn::Is(label1));

                ref_nodes[label1].push((label2, 0));
                ref_nodes[label2].push((label1, 0));
            }
            IRTypeHint::Operator(typ, typ1, op, typ2) => {
                type_nodes[typ1].push(Conn::Operator(typ, op, typ2));
                type_nodes[typ2].push(Conn::Operator(typ, op, typ1));

                try_set_ref(typ1, 0, &mut known_refs, &mut queue);
                try_set_ref(typ2, 0, &mut known_refs, &mut queue);
            }
            IRTypeHint::IsRef(typ1, typ2) => {
                // typ1 = &typ2
                type_nodes[typ2].push(Conn::Is(typ1));
                type_nodes[typ1].push(Conn::Is(typ2));

                ref_nodes[typ1].push((typ2, -1));
                ref_nodes[typ2].push((typ1, 1));
            }
            IRTypeHint::Struct(typ, structure, types) => {
                for arg_type in &types {
                    type_nodes[*arg_type].push(Conn::Struct(typ, structure, types.clone()));
                }
                try_set_ref(typ, 0, &mut known_refs, &mut queue);
            }
            IRTypeHint::IsField(typ1, typ2, field) => {
                type_nodes[typ2].push(Conn::IsField(typ1, field));
                try_set_ref(typ2, 0, &mut known_refs, &mut queue);
            }
            IRTypeHint::AutoRef(label, typ1, typ2) => {
                type_nodes[typ1].push(Conn::Is(typ2));
                type_nodes[typ2].push(Conn::Is(typ1));
                // no ref hints, since we know nothing about references
            }
        }
    }

    loop {
        let node =
            if let Some(node) = queue.pop() {
                node
            } else {
                break;
            };

        // loop for type deduction
        if let Some(node_type) = known_types[node].clone() {
            'neighbour_loop: for ne in &type_nodes[node] {
                match ne {
                    Conn::Is(ne) => {
                        try_set_type(*ne, node_type.clone(), &mut known_types, &mut queue);
                    }
                    Conn::Operator(ne, op, typ2) => {
                        if let Some(node_type2) = known_types[*typ2].clone() {
                            let ir_type = operator_map[&(node_type.clone(), *op, node_type2)].clone();
                            let (ir_type, ref_depth) = deref_type(ir_type);
                            try_set_type(*ne, ir_type, &mut known_types, &mut queue);
                            try_set_ref(*ne, ref_depth, &mut known_refs, &mut queue);
                        }
                    }
                    Conn::Struct(typ, structure, args) => {
                        // only deduce type when all arguments are known
                        let mut ir_args = Vec::new();
                        for arg in args {
                            if let Some(arg_type) = &known_types[*arg] && let Some(ref_depth) = known_refs[*arg] {
                                ir_args.push(ref_type(arg_type.clone(), ref_depth));
                            } else {
                                continue 'neighbour_loop;
                            }
                        }

                        let ir_type = IRType::Struct(*structure, ir_args);
                        try_set_type(*typ, ir_type, &mut known_types, &mut queue);
                    }
                    Conn::IsField(typ, field) => {
                        let (struct_label, struct_args) = match &known_types[node] {
                            Some(IRType::Struct(label, args)) => (*label, args),
                            _ => panic!()
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

                        try_set_type(*typ, ir_type, &mut known_types, &mut queue);
                        try_set_ref(*typ, ref_depth, &mut known_refs, &mut queue);
                    }
                }
            }
        }

        // loop for ref deduction
        if let Some(node_ref) = known_refs[node] {
            for (ne, delta) in &ref_nodes[node] {
                try_set_ref(*ne, node_ref + *delta, &mut known_refs, &mut queue);
            }
        }
    }

    println!("{:?}", known_types);
    println!("{:?}", known_refs);
    for (typ, ref_depth) in known_types.into_iter().zip(known_refs) {
        ir.types.push(ref_type(typ.unwrap(), ref_depth.unwrap()));
    }
}
