use crate::compiler::error::FilePosition;
use crate::compiler::tokenizer::TokenBlock;
use std::fmt::{Debug, Formatter};

#[derive(Debug)]
pub struct Ast {
    pub functions: Vec<(ASTFunctionSignature, ASTBlock)>,
    pub structs: Vec<ASTStructDeclaration>,
    // variables are (name, type_hint, initial_value, ident position)
    pub global_variables: Vec<(String, ASTType, Option<ASTExpression>, FilePosition)>,
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
    // Same as block, except its scope leaks into the parent block
    SemiBlock {
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
    For {
        iterator: String,
        element: ASTExpression,
        block: ASTBlock,
        pos: FilePosition,
    },
}

#[derive(Clone)]
pub struct ASTBlock {
    pub children: Vec<ASTStatement>,
}

impl Debug for ASTBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("[\n")?;
        for child in &self.children {
            child.fmt(f)?;
            f.write_str("\n")?;
        }
        f.write_str("]\n")?;
        Ok(())
    }
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
    Comma, // for tuples
    DotDot, // .. for ranges
}

#[derive(Debug, Clone)]
pub struct ASTExpression {
    pub kind: ASTExpressionKind,
    pub pos: FilePosition,
}

impl ASTExpression {
    pub const fn new(kind: ASTExpressionKind, pos: FilePosition) -> Self {
        Self { kind, pos }
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
    TupleAccess {
        expression: Box<ASTExpression>,
        field_index: usize,
    },
    TupleInitialization(Vec<ASTExpression>),
    MethodCall {
        expression: Box<ASTExpression>,
        call: ASTFunctionCall,
    },
    Index {
        expression: Box<ASTExpression>,
        arguments: Vec<ASTExpression>,
    },
    Dereference(Box<ASTExpression>),
    BinaryOperation {
        expression1: Box<ASTExpression>,
        operator: ASTOperator,
        expression2: Box<ASTExpression>,
    },
    // wrapper that automatically decides how many references/dereferences should the expression have
    // using this is worse for type checker, since it has less information but better for the user,
    // since there is no need for manual referencing
    AutoRef(Box<ASTExpression>),
    // wrapper that determines type for expression
    TypeHint {
        expression: Box<ASTExpression>,
        type_hint: ASTType,
    },
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
    // the compiler first collects all method signatures and raw blocks and later parses them,
    // because parsing stage already needs to know all declared structs
    pub pre_methods: Vec<(ASTFunctionSignature, TokenBlock)>,
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
    Tuple(Vec<Self>, FilePosition),
}

impl ASTType {
    pub const fn get_pos(&self) -> FilePosition {
        match self {
            Self::Any(pos) | Self::Primitive(_, pos) | Self::Reference(_, pos) | Self::Identifier(_, pos, _) | Self::Tuple(_, pos) => *pos,
        }
    }
}
