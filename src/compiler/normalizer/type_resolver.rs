use crate::compiler::normalizer::ir::IR;

pub fn resolve_types(ir: &mut IR, num_types: usize) {
    let mut known_types = Vec::new();
    for _ in 0..num_types {
        known_types.push(None);
    }

    println!("{:?}", known_types);
    for typ in known_types {
        ir.types.push(typ.unwrap());
    }
}