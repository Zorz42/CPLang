use std::collections::{HashMap, VecDeque};
use crate::compiler::normalizer::ir::{IROperator, IRPrimitiveType, IRType, IRTypeHint, IRTypeLabel, IR};

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
    let mut nodes_op = Vec::new();
    for _ in 0..num_types {
        nodes.push(Vec::new());
        nodes_op.push(Vec::new());
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
                nodes[label1].push(label2);
                nodes[label2].push(label1);
            }
            IRTypeHint::Operator(typ, typ1, op, typ2) => {
                nodes_op[typ1].push((typ, typ2, op));
                nodes_op[typ2].push((typ, typ1, op));
            }
        }
    }

    loop {
        let node =
            if let Some(node) = queue.pop_front() {
                node
            } else {
                break;
            };
        let node_type = known_types[node].as_ref().unwrap().clone();

        for ne in &nodes[node] {
            if let Some(ne_typ) = &known_types[*ne] {
                if *ne_typ != node_type {
                    panic!();
                }
                continue;
            }

            known_types[*ne] = Some(node_type.clone());
            queue.push_back(*ne);
        }

        for (ne, typ2, op) in &nodes_op[node] {
            if let Some(node_type2) = known_types[*typ2].clone() {
                let res_type = operator_map[&(node_type.clone(), *op, node_type2)].clone();
                if known_types[*ne].is_some() && known_types[*ne].as_ref() != Some(&res_type) {
                    panic!();
                }
                if known_types[*ne].is_none() {
                    known_types[*ne] = Some(res_type);
                    queue.push_back(*ne);
                }
            }
        }
    }

    println!("{:?}", known_types);
    for typ in known_types {
        ir.types.push(typ.unwrap());
    }
}