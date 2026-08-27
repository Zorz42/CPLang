use crate::compiler::error::FilePosition;
use crate::compiler::tokenizer::TokenBlock;
use std::fmt::{Debug, Formatter};

#[derive(Debug)]
pub struct Ast {
    pub functions: Vec<(ASTFunctionSignature, ASTBlock)>,
    pub structs: Vec<ASTStructDeclaration>,
    // variables are (name, type_hint, initial_value, ident position)
    pub global_variables: Vec<ASTGlobalVariable>,
}

#[derive(Debug)]
pub struct ASTGlobalVariable {
    pub name: String,
    pub type_hint: ASTType,
    pub initial_value: Option<ASTExpression>,
    pub pos: FilePosition,
    pub file_idx: usize,
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
        else_block: ASTBlock,
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
    Break {
        depth: i32,
        pos: FilePosition,
    },
    Continue {
        depth: i32,
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
    Plus,        // +
    Minus,       // -
    Mul,         // *
    Div,         // /
    Mod,         // %
    Equals,      // ==
    NotEquals,   // !=
    Greater,     // >
    Lesser,      // <
    GreaterEq,   // >=
    LesserEq,    // <=
    And,         // &&
    Or,          // ||
    Comma,       // , for tuples
    DotDot,      // .. for ranges
    PlusEquals,  // +=
    MinusEquals, // -=
    MulEquals,   // *=
    DivEquals,   // /=
    ModEquals,   // %=
}

#[derive(Debug, Clone)]
pub enum ASTUnaryOperator {
    Minus,     // -
    Not,       // !
    Increment, // ++
    Decrement, // --
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
    Integer32(i32),
    Integer64(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Char(char),
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
    UnaryOperation {
        expression: Box<ASTExpression>,
        operator: ASTUnaryOperator,
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
    pub file_idx: usize,
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

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum PrimitiveType {
    I32,
    I64,
    F32,
    F64,
    Bool,
    String,
    Char,
    Void,
}

#[derive(Debug, Clone)]
pub enum ASTType {
    Any(FilePosition),
    Primitive(PrimitiveType, FilePosition),
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
