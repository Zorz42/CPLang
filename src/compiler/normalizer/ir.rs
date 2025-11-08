use std::fmt::Debug;

pub type IRFunctionLabel = usize;
pub type IRVariableLabel = usize;
pub type IRTypeLabel = usize;
pub type IRStructLabel = usize;
pub type IRFieldLabel = usize;
pub type IRAutoRefLabel = usize;

pub struct IR {
    pub structs: Vec<IRStruct>,
    pub functions: Vec<IRFunction>,
    pub types: Vec<IRType>,
    pub variable_types: Vec<IRTypeLabel>,
    pub autorefs: Vec<i32>,
    pub main_function: IRFunctionLabel,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum IRPrimitiveType {
    I32,
    I64,
    F32,
    F64,
    Bool,
    String,
    Void,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum IRType {
    Primitive(IRPrimitiveType),
    Reference(Box<IRType>),
    Tuple(Vec<IRType>),
    Enum(Vec<IRType>),
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

#[derive(Debug)]
pub enum IRExpression {
    BinaryOperation(IROperator, Box<(IRExpression, IRTypeLabel, IRExpression, IRTypeLabel)>),
    Constant(IRConstant),
    FunctionCall(IRFunctionLabel, Vec<IRExpression>),
    FieldAccess(Box<IRExpression>, IRFieldLabel),
    Dereference(Box<IRExpression>),
    StructInitialization(IRStructLabel, Vec<IRTypeLabel>, Vec<IRExpression>),
    Reference(Box<IRExpression>),
    Variable(IRVariableLabel),
    AutoRef(IRAutoRefLabel, Box<IRExpression>),
}

pub struct IRBlock {
    pub statements: Vec<IRStatement>,
}

#[derive(Debug)]
pub enum IRStatement {
    Block(IRBlock),
    If(IRExpression, IRBlock, Option<IRBlock>),
    While(IRExpression, IRBlock),
    Expression(IRExpression),
    Print(IRExpression, IRTypeLabel),
    Return(Option<IRExpression>),
    Assignment(IRExpression, IRExpression),
}

pub struct IRFunction {
    pub arguments: Vec<IRVariableLabel>,
    pub variables: Vec<IRVariableLabel>,
    pub block: IRBlock,
    pub ret_type: IRTypeLabel,
    pub label: IRFunctionLabel,
}

#[derive(Debug, Clone)]
pub struct IRStruct {
    pub fields: Vec<IRFieldLabel>,
}