use std::collections::{HashMap, VecDeque};
use crate::compiler::normalizer::ir::{IROperator, IRPrimitiveType, IRType, IRTypeLabel, IR};

pub enum IRTypeHint {
    Is(IRTypeLabel, IRPrimitiveType),
    Equal(IRTypeLabel, IRTypeLabel),
    // arg0 = arg1 + arg3, + is arg2
    Operator(IRTypeLabel, IRTypeLabel, IROperator, IRTypeLabel),
    // arg0 = &arg1
    IsRef(IRTypeLabel, IRTypeLabel),
}

enum Conn {
    // node = arg1
    Is(IRTypeLabel),

    // node = &arg1
    IsRef(IRTypeLabel),

    // node = :arg1
    IsDeref(IRTypeLabel),

    // node + arg1 = arg3 (+ is arg2)
    Operator(IRTypeLabel, IROperator, IRTypeLabel),
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

pub fn resolve_types(ir: &mut IR, num_types: usize, type_hints: Vec<IRTypeHint>) {
    let mut known_types = Vec::new();
    for _ in 0..num_types {
        known_types.push(None);
    }

    let mut nodes = Vec::new();
    for _ in 0..num_types {
        nodes.push(Vec::new());
    }

    let operator_map = setup_operator_map();

    let mut queue = VecDeque::<IRTypeLabel>::new();
    for hint in type_hints {
        match hint {
            IRTypeHint::Is(label, typ) => {
                let ir_type = Some(IRType::Primitive(typ.clone()));
                assert!(known_types[label].is_none() || known_types[label] == ir_type);
                known_types[label] = ir_type;
                queue.push_back(label);
            }
            IRTypeHint::Equal(label1, label2) => {
                nodes[label1].push(Conn::Is(label2));
                nodes[label2].push(Conn::Is(label1));
            }
            IRTypeHint::Operator(typ, typ1, op, typ2) => {
                nodes[typ1].push(Conn::Operator(typ, op, typ2));
                nodes[typ2].push(Conn::Operator(typ, op, typ1));
            }
            IRTypeHint::IsRef(typ1, typ2) => {
                // typ1 = &typ2
                nodes[typ2].push(Conn::IsRef(typ1));
                nodes[typ1].push(Conn::IsDeref(typ2));
            }
        }
    }

    let try_set = |
        label: IRTypeLabel,
        typ: IRType,
        known_types: &mut Vec<Option<IRType>>,
        queue: &mut VecDeque<IRTypeLabel>
    | {
        if known_types[label].is_some() && known_types[label].as_ref() != Some(&typ) {
            panic!();
        }
        if known_types[label].is_none() {
            known_types[label] = Some(typ);
            queue.push_back(label);
        }
    };

    loop {
        let node =
            if let Some(node) = queue.pop_front() {
                node
            } else {
                break;
            };
        let node_type = known_types[node].as_ref().unwrap().clone();

        for ne in &nodes[node] {
            match ne {
                Conn::Is(ne) => {
                    try_set(*ne, node_type.clone(), &mut known_types, &mut queue);
                }
                Conn::IsRef(ne) => {
                    try_set(*ne, IRType::Reference(Box::new(node_type.clone())), &mut known_types, &mut queue);
                }
                Conn::IsDeref(ne) => {
                    let typ = match node_type.clone() {
                        IRType::Reference(typ) => typ,
                        _ => panic!()
                    };
                    try_set(*ne, *typ, &mut known_types, &mut queue);
                }
                Conn::Operator(ne, op, typ2) => {
                    if let Some(node_type2) = known_types[*typ2].clone() {
                        let res_type = operator_map[&(node_type.clone(), *op, node_type2)].clone();
                        try_set(*ne, res_type.clone(), &mut known_types, &mut queue);
                    }
                }
            }
        }
    }

    println!("{:?}", known_types);
    for typ in known_types {
        ir.types.push(typ.unwrap());
    }
}