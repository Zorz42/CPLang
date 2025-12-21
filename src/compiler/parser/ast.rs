use crate::compiler::error::FilePosition;

#[derive(Debug)]
pub struct Ast {
    pub functions: Vec<(ASTFunctionSignature, ASTBlock)>,
    pub structs: Vec<ASTStructDeclaration>,
}

#[derive(Debug, Clone)]
pub struct ASTFunctionCall {
    pub name: String,
    pub arguments: Vec<ASTExpression>,
    pub template_arguments: Vec<ASTType>,
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

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
pub enum ASTOperator {
    Plus,
    Mul,
    Div,
    Equals,
    NotEquals,
    Greater,
    Lesser,
    GreaterEq,
    LesserEq,
    Minus,
}

#[derive(Debug, Clone)]
pub struct ASTExpression {
    pub kind: ASTExpressionKind,
    pub pos: FilePosition,
    pub type_hint: ASTType,
}

impl ASTExpression {
    pub const fn no_hint(kind: ASTExpressionKind, pos: FilePosition) -> Self {
        Self {
            kind,
            type_hint: ASTType::Any(pos),
            pos,
        }
    }
}

#[rustfmt::skip]
#[derive(Debug, Clone)]
pub enum ASTExpressionKind {
    Integer(i32),
    Float(f32),
    String(String),
    Boolean(bool),
    Variable(String),
    Reference(Box<ASTExpression>),
    FunctionCall(ASTFunctionCall),
    StructInitialization {
        name: String,
        fields: Vec<ASTExpression>,
        template_arguments: Vec<ASTType>,
    },
    FieldAccess {
        expression: Box<ASTExpression>,
        field_name: String,
    },
    MethodCall {
        expression: Box<ASTExpression>,
        call: ASTFunctionCall,
    },
    Dereference(Box<ASTExpression>),
    BinaryOperation {
        expression1: Box<ASTExpression>,
        operator: ASTOperator,
        expression2: Box<ASTExpression>,
    },
    AutoRef(Box<ASTExpression>),
}

#[derive(Debug, Clone)]
pub struct ASTFunctionSignature {
    pub name: String,
    pub args: Vec<(String, ASTType, FilePosition)>,
    pub template: Vec<(String, FilePosition)>,
    // template can have extra hidden arguments
    pub num_template_args: usize,
    pub pos: FilePosition,
}

#[derive(Debug, Clone)]
pub struct ASTStructDeclaration {
    pub name: String,
    pub fields: Vec<(String, ASTType)>,
    pub methods: Vec<(ASTFunctionSignature, ASTBlock)>,
    pub template: Vec<(String, FilePosition)>,
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
    Reference(Box<Self>, FilePosition),
    Identifier(String, FilePosition, Vec<Self>),
}

impl ASTType {
    pub const fn get_pos(&self) -> FilePosition {
        match self {
            Self::Any(pos) | Self::Primitive(_, pos) | Self::Reference(_, pos) | Self::Identifier(_, pos, _) => *pos,
        }
    }
}
