use std::cmp::PartialEq;
use std::collections::VecDeque;
use crate::compiler::normalizer::ir::{IRType, IRTypeHint, IRTypeLabel, IR};

pub fn resolve_types(ir: &mut IR, num_types: usize, type_hints: &Vec<IRTypeHint>) {
    let mut known_types = Vec::new();
    for _ in 0..num_types {
        known_types.push(None);
    }

    let mut nodes = Vec::new();
    for i in 0..num_types {
        nodes.push(Vec::new());
    }

    let mut queue = VecDeque::<IRTypeLabel>::new();
    for hint in type_hints {
        match hint {
            IRTypeHint::Is(label, typ) => {
                let ir_type = Some(IRType::Primitive(typ.clone()));
                assert!(known_types[*label].is_none() || known_types[*label] == ir_type);
                known_types[*label] = ir_type;
                queue.push_back(*label);
            }
            IRTypeHint::Eq(label1, label2) => {
                nodes[*label1].push(*label2);
                nodes[*label2].push(*label1);
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
        let node_typ = known_types[node].as_ref().unwrap().clone();
        
        for ne in &nodes[node] {
            if let Some(ne_typ) = &known_types[*ne] {
                if *ne_typ != node_typ {
                    panic!();
                }
                continue;
            }
            
            known_types[*ne] = Some(node_typ.clone());
            queue.push_back(*ne);
        }
    }

    println!("{:?}", known_types);
    for typ in known_types {
        ir.types.push(typ.unwrap());
    }
}