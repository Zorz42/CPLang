use crate::compiler::error::FilePosition;

#[derive(Debug)]
pub struct AST {
    pub functions: Vec<(ASTFunctionSignature, ASTBlock)>,
    pub structs: Vec<StructDeclaration>,
}

#[derive(Debug, Clone)]
pub enum ASTStatement {
    Assignment(Assignment),
    Block(ASTBlock),
    Expression(ASTExpression),
    Print(PrintStatement),
    Return(Option<ASTExpression>, FilePosition),
    If(IfStatement),
    While(WhileStatement),
}

#[derive(Debug, Clone)]
pub struct ASTBlock {
    pub children: Vec<ASTStatement>,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum ASTOperator {
    Plus,
    Mul,
    Div,
    Equals,
    NotEquals,
    Greater,
    Less,
    GreaterEquals,
    LessEquals,
    Minus,
}

#[derive(Debug, Clone)]
pub enum ASTExpression {
    Integer(i32),
    Float(f32),
    String(String),
    Boolean(bool),
    Variable(String, FilePosition),
    Reference(Box<ASTExpression>, FilePosition),
    FunctionCall(String, Vec<ASTExpression>),
    StructInitialization(String, Vec<ASTExpression>),
    FieldAccess(Box<ASTExpression>, String, FilePosition),
    MethodCall(Box<ASTExpression>, FilePosition, String, Vec<ASTExpression>),
    Dereference(Box<ASTExpression>, FilePosition),
    BinaryOperation(Box<ASTExpression>, ASTOperator, Box<ASTExpression>, FilePosition),
    AutoRef(Box<ASTExpression>),
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTFunctionSignature {
    pub name: String,
    pub args: Vec<(String, ASTType)>,
}

#[derive(Debug, Clone)]
pub struct PrintStatement {
    pub values: Vec<ASTExpression>,
}
#[derive(Clone, Debug)]
pub struct IfStatement {
    pub condition: ASTExpression,
    pub block: ASTBlock,
    pub else_block: Option<ASTBlock>,
}

#[derive(Clone, Debug)]
pub struct WhileStatement {
    pub condition: ASTExpression,
    pub block: ASTBlock,
}

#[derive(Debug, Clone)]
pub struct StructDeclaration {
    pub name: String,
    pub fields: Vec<String>,
    pub methods: Vec<(ASTFunctionSignature, ASTBlock)>,
}

#[derive(Debug, Clone)]
pub enum Assignment {
    Assign(ASTExpression, ASTExpression, FilePosition),
    Increase(ASTExpression, ASTExpression, FilePosition),
    Decrease(ASTExpression, ASTExpression, FilePosition),
    Increment(ASTExpression, FilePosition),
    Decrement(ASTExpression, FilePosition),
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum ASTPrimitiveType {
    I32,
    I64,
    F32,
    F64,
    Bool,
    String,
    Void,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum ASTType {
    Any,
    Primitive(ASTPrimitiveType),
    Reference(Box<ASTType>),
    Struct(String),
}