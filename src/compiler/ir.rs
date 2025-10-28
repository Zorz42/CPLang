use crate::compiler::parser::expression::Expression;

type IRFunctionLabel = usize;
type IRVariableLabel = usize;
type IRGenericLabel = usize;
type IRStructLabel = usize;
type IRFieldLabel = usize;

enum IRPrimitiveType {
    I32,
    I64,
    F32,
    F64,
    Bool,
    String,
    Void,
}

enum IRType {
    Primitive(IRPrimitiveType),
    Reference(Box<IRType>),
    Tuple(Vec<IRType>),
    Enum(Vec<IRType>),
    Struct(Vec<(String, IRType)>),
    Generic(IRGenericLabel),
}

enum IROperator {
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

enum IRConstant {
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
}

enum IRExpression {
    Operator(IROperator, Box<(IRExpression, IRExpression)>),
    Constant(IRConstant),
    FunctionCall(IRFunctionLabel, Box<IRExpression>),
    FieldAccess(Box<IRExpression>, IRFieldLabel),
    Dereference(Box<IRExpression>),
    StructInitialization(IRStructLabel, Vec<Expression>),
    Reference(Box<IRExpression>),
    Variable(IRVariableLabel),
}

struct IRBlock {
    variables: Vec<IRVariableLabel>,
    statements: Vec<IRStatement>,
}

enum IRStatement {
    Block(IRBlock),
    If(IRExpression, IRBlock),
    While(IRExpression, IRBlock),
    Expression(IRExpression),
    Print(IRExpression),
    Return(Option<IRExpression>),
}