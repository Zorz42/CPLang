use std::fmt::Debug;

// Function is a generic function. When you call it, it is reduced into
// instance where all argument types are known

pub type IRInstanceLabel = usize;
pub type IRVariableLabel = usize;
pub type IRTypeLabel = usize;
pub type IRStructLabel = usize;
pub type IRFieldLabel = usize;
pub type IRAutoRefLabel = usize;

pub struct IR {
    pub structs: Vec<IRStruct>,
    pub instances: Vec<IRInstance>,
    pub types: Vec<IRType>,
    pub variable_types: Vec<IRTypeLabel>,
    pub autorefs: Vec<i32>,
    pub main_function: IRInstanceLabel,
}

#[derive(Debug)]
pub enum BuiltinFunctionCall {
    Alloc {
        typ: IRTypeLabel,
        num: Box<IRExpression>,
    },
    Index {
        arr: Box<IRExpression>,
        idx: Box<IRExpression>,
    },
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub enum IRPrimitiveType {
    I32,
    I64,
    F32,
    F64,
    Bool,
    String,
    Void,
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum IRType {
    Primitive(IRPrimitiveType),
    Reference(Box<IRType>),
    Struct(IRStructLabel, Vec<IRType>),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum IROperator {
    Plus,
    Minus,
    Mul,
    Div,
    Equals,
    NotEquals,
    Greater,
    GreaterOrEq,
    Lesser,
    LesserOrEq,
}

#[derive(Debug)]
pub enum IRConstant {
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
}

#[rustfmt::skip]
#[derive(Debug)]
pub enum IRExpression {
    BinaryOperation {
        operator: IROperator,
        expression1: Box<IRExpression>,
        type1_label: IRTypeLabel,
        expression2: Box<IRExpression>,
        type2_label: IRTypeLabel,
    },
    Constant {
        constant: IRConstant,
    },
    InstanceCall {
        instance_label: IRInstanceLabel,
        instance_arguments: Vec<IRExpression>,
    },
    BuiltinFunctionCall(BuiltinFunctionCall),
    FieldAccess {
        expression: Box<IRExpression>,
        field_label: IRFieldLabel,
    },
    Dereference {
        expression: Box<IRExpression>,
    },
    StructInitialization {
        struct_label: IRStructLabel,
        fields_type_labels: Vec<IRTypeLabel>,
        field_values: Vec<IRExpression>,
    },
    Reference {
        expression: Box<IRExpression>,
    },
    Variable {
        variable_label: IRVariableLabel,
    },
    AutoRef {
        autoref_label: IRAutoRefLabel,
        expression: Box<IRExpression>,
    },
}

pub struct IRBlock {
    pub statements: Vec<IRStatement>,
}

#[rustfmt::skip]
#[derive(Debug)]
pub enum IRStatement {
    Block {
        block: IRBlock,
    },
    If {
        condition: IRExpression,
        block: IRBlock,
        else_block: Option<IRBlock>,
    },
    While {
        condition: IRExpression,
        block: IRBlock,
    },
    Expression {
        expr: IRExpression,
    },
    Print {
        expr: IRExpression,
        type_label: IRTypeLabel,
    },
    Return {
        return_value: Option<IRExpression>,
    },
    Assignment {
        assign_to: IRExpression,
        value: IRExpression,
    },
}

pub struct IRInstance {
    pub arguments: Vec<IRVariableLabel>,
    pub variables: Vec<IRVariableLabel>,
    pub block: IRBlock,
    pub ret_type: IRTypeLabel,
    pub label: IRInstanceLabel,
}

#[derive(Debug, Clone)]
pub struct IRStruct {
    pub fields: Vec<IRFieldLabel>,
}
