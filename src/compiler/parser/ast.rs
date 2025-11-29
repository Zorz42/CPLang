use crate::compiler::error::FilePosition;

#[derive(Debug)]
pub struct Ast {
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
    Integer(i32, FilePosition),
    Float(f32, FilePosition),
    String(String, FilePosition),
    Boolean(bool, FilePosition),
    Variable(String, FilePosition),
    Reference {
        expression: Box<ASTExpression>,
        pos: FilePosition,
    },
    FunctionCall {
        name: String,
        arguments: Vec<ASTExpression>,
        pos: FilePosition,
    },
    StructInitialization {
        name: String,
        fields: Vec<ASTExpression>,
        pos: FilePosition,
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

impl ASTExpression {
    pub fn get_pos(&self) -> FilePosition {
        match self {
            Self::Integer(_, pos) |
            Self::Float(_, pos) |
            Self::String(_, pos) |
            Self::Boolean(_, pos) |
            Self::Variable(_, pos) |
            Self::Reference { pos, .. } |
            Self::FunctionCall { pos, .. } |
            Self::StructInitialization { pos, .. } |
            Self::FieldAccess { pos, .. } |
            Self::MethodCall { pos, .. } |
            Self::Dereference { pos, .. } |
            Self::BinaryOperation { pos, .. } => pos.clone(),
            Self::AutoRef { expression } => expression.get_pos(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ASTFunctionSignature {
    pub name: String,
    pub args: Vec<(String, ASTType, FilePosition)>,
    pub template: Vec<(String, FilePosition)>,
    pub pos: FilePosition,
}

#[derive(Debug, Clone)]
pub struct ASTStructDeclaration {
    pub name: String,
    pub fields: Vec<(String, ASTType)>,
    pub methods: Vec<(ASTFunctionSignature, ASTBlock)>,
}

#[derive(Debug, Clone)]
pub enum ASTPrimitiveType {
    I32,
    I64,
    F32,
    F64,
    Bool,
    String,
    Void,
}

#[derive(Debug, Clone)]
pub enum ASTType {
    Any(FilePosition),
    Primitive(ASTPrimitiveType, FilePosition),
    Reference(Box<ASTType>, FilePosition),
    Struct(String, FilePosition),
}

impl ASTType {
    pub fn get_pos(&self) -> FilePosition {
        match self {
            Self::Any(pos) |
            Self::Primitive(_, pos) |
            Self::Reference(_, pos) |
            Self::Struct(_, pos) => pos.clone()
        }
    }
}
