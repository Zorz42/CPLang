use crate::compiler::normalizer::ir::{IROperator, IRPrimitiveType, IRType};
use std::collections::HashMap;

#[allow(clippy::type_complexity)]
pub fn init_default_operators() -> HashMap<(IRType, IROperator, IRType), Box<dyn Fn(String, String) -> String>> {
    let mut res: HashMap<(IRType, IROperator, IRType), Box<dyn Fn(String, String) -> String>> = HashMap::new();

    let c_plus = |a, b| format!("{a} + {b}");
    let c_minus = |a, b| format!("{a} - {b}");
    let c_mul = |a, b| format!("{a} * {b}");
    let c_div = |a, b| format!("{a} / {b}");
    let c_equal = |a, b| format!("{a} == {b}");
    let c_not_equal = |a, b| format!("{a} != {b}");
    let c_greater = |a, b| format!("{a} > {b}");
    let c_greater_or_eq = |a, b| format!("{a} >= {b}");
    let c_lesser = |a, b| format!("{a} < {b}");
    let c_lesser_or_eq = |a, b| format!("{a} <= {b}");
    res.insert(
        (
            IRType::Primitive(IRPrimitiveType::I32),
            IROperator::Plus,
            IRType::Primitive(IRPrimitiveType::I32),
        ),
        Box::new(c_plus),
    );
    res.insert(
        (
            IRType::Primitive(IRPrimitiveType::I32),
            IROperator::Minus,
            IRType::Primitive(IRPrimitiveType::I32),
        ),
        Box::new(c_minus),
    );
    res.insert(
        (
            IRType::Primitive(IRPrimitiveType::I32),
            IROperator::Mul,
            IRType::Primitive(IRPrimitiveType::I32),
        ),
        Box::new(c_mul),
    );
    res.insert(
        (
            IRType::Primitive(IRPrimitiveType::I32),
            IROperator::Div,
            IRType::Primitive(IRPrimitiveType::I32),
        ),
        Box::new(c_div),
    );
    res.insert(
        (
            IRType::Primitive(IRPrimitiveType::I32),
            IROperator::Equals,
            IRType::Primitive(IRPrimitiveType::I32),
        ),
        Box::new(c_equal),
    );
    res.insert(
        (
            IRType::Primitive(IRPrimitiveType::I32),
            IROperator::NotEquals,
            IRType::Primitive(IRPrimitiveType::I32),
        ),
        Box::new(c_not_equal),
    );
    res.insert(
        (
            IRType::Primitive(IRPrimitiveType::I32),
            IROperator::Greater,
            IRType::Primitive(IRPrimitiveType::I32),
        ),
        Box::new(c_greater),
    );
    res.insert(
        (
            IRType::Primitive(IRPrimitiveType::I32),
            IROperator::GreaterEq,
            IRType::Primitive(IRPrimitiveType::I32),
        ),
        Box::new(c_greater_or_eq),
    );
    res.insert(
        (
            IRType::Primitive(IRPrimitiveType::I32),
            IROperator::Lesser,
            IRType::Primitive(IRPrimitiveType::I32),
        ),
        Box::new(c_lesser),
    );
    res.insert(
        (
            IRType::Primitive(IRPrimitiveType::I32),
            IROperator::LesserEq,
            IRType::Primitive(IRPrimitiveType::I32),
        ),
        Box::new(c_lesser_or_eq),
    );

    res
}
