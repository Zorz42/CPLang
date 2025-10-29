use std::fmt::{Debug, Formatter};

pub type IRFunctionLabel = usize;
pub type IRVariableLabel = usize;
pub type IRTypeLabel = usize;
pub type IRStructLabel = usize;
pub type IRFieldLabel = usize;

#[derive(Debug)]
pub enum IRPrimitiveType {
    I32,
    I64,
    F32,
    F64,
    Bool,
    String,
    Void,
}

#[derive(Debug)]
pub enum IRType {
    Primitive(IRPrimitiveType),
    Reference(Box<IRType>),
    Tuple(Vec<IRType>),
    Enum(Vec<IRType>),
    Struct(Vec<(String, IRType)>),
}

#[derive(Debug)]
pub enum IROperator {
    Plus,
    Minus,
    Times,
    Divide,
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
    BinaryOperation(IROperator, Box<(IRExpression, IRExpression)>),
    Constant(IRConstant),
    FunctionCall(IRFunctionLabel, Vec<IRExpression>),
    FieldAccess(Box<IRExpression>, IRFieldLabel),
    Dereference(Box<IRExpression>),
    StructInitialization(IRStructLabel, Vec<IRExpression>),
    Reference(Box<IRExpression>),
    Variable(IRVariableLabel),
}

pub struct IRBlock {
    pub variables: Vec<IRVariableLabel>,
    pub statements: Vec<IRStatement>,
}

impl Debug for IRBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Variables: {:?}", self.variables)?;
        writeln!(f, "Statements:")?;
        for statement in &self.statements {
            writeln!(f, "{:?}", statement)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum IRStatement {
    Block(IRBlock),
    If(IRExpression, IRBlock),
    While(IRExpression, IRBlock),
    Expression(IRExpression),
    Print(IRExpression),
    Return(Option<IRExpression>),
    Assignment(IRExpression, IRExpression),
}

#[derive(Debug)]
pub struct IRFunction {
    pub arguments: Vec<IRVariableLabel>,
    pub block: IRBlock,
}

#[derive(Debug)]
pub struct IRStruct {
    pub fields: Vec<IRFieldLabel>,
}