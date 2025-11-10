use crate::compiler::error::FilePosition;

#[derive(Debug)]
pub struct AST {
    pub functions: Vec<(ASTFunctionSignature, ASTBlock)>,
    pub structs: Vec<ASTStructDeclaration>,
}

#[rustfmt::skip]
#[derive(Debug, Clone)]
pub enum ASTStatement {
    Block {
        block: ASTBlock,
    },
    Expression {
        expression: ASTExpression,
    },
    Assignment {
        assign_to: ASTExpression,
        value: ASTExpression,
        pos: FilePosition,
    },
    // just like a += b but for any operator
    AssignmentOperator {
        assign_to: ASTExpression,
        value: ASTExpression,
        operator: ASTOperator,
        pos: FilePosition,
    },
    AssignmentIncrement {
        assign_to: ASTExpression,
        pos: FilePosition,
    },
    AssignmentDecrement {
        assign_to: ASTExpression,
        pos: FilePosition,
    },
    Print {
        values: Vec<ASTExpression>,
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

#[rustfmt::skip]
#[derive(Debug, Clone)]
pub enum ASTExpression {
    Integer(i32),
    Float(f32),
    String(String),
    Boolean(bool),
    Variable(String, FilePosition),
    Reference {
        expression: Box<ASTExpression>,
        pos: FilePosition,
    },
    FunctionCall {
        name: String,
        arguments: Vec<ASTExpression>,
    },
    StructInitialization {
        name: String,
        fields: Vec<ASTExpression>,
    },
    FieldAccess {
        expression: Box<ASTExpression>,
        field_name: String,
        pos: FilePosition,
    },
    MethodCall {
        expression: Box<ASTExpression>,
        method_name: String,
        arguments: Vec<ASTExpression>,
        pos: FilePosition,
    },
    Dereference {
        expression: Box<ASTExpression>,
        pos: FilePosition,
    },
    BinaryOperation {
        expression1: Box<ASTExpression>,
        operator: ASTOperator,
        expression2: Box<ASTExpression>,
        pos: FilePosition,
    },
    AutoRef {
        expression: Box<ASTExpression>,
    },
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
