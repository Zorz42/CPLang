use crate::compiler::error::FilePosition;

#[derive(Debug)]
pub struct AST {
    pub functions: Vec<(ASTFunctionSignature, ASTBlock)>,
    pub structs: Vec<ASTStructDeclaration>,
}

#[derive(Debug, Clone)]
pub enum ASTStatement {
    Block(ASTBlock),
    Expression(ASTExpression),
    Assignment(ASTExpression, ASTExpression, FilePosition),
    AssignmentIncrease(ASTExpression, ASTExpression, FilePosition),
    AssignmentDecrease(ASTExpression, ASTExpression, FilePosition),
    AssignmentIncrement(ASTExpression, FilePosition),
    AssignmentDecrement(ASTExpression, FilePosition),
    Print {
        values: Vec<ASTExpression>
    },
    Return {
        return_value: Option<ASTExpression>,
        pos: FilePosition,
    },
    If {
        condition: ASTExpression,
        block: ASTBlock,
        else_block: Option<ASTBlock>,
    },
    While {
        condition: ASTExpression,
        block: ASTBlock,
    },
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
pub struct ASTStructDeclaration {
    pub name: String,
    pub fields: Vec<String>,
    pub methods: Vec<(ASTFunctionSignature, ASTBlock)>,
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