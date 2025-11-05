use crate::compiler::error::CompilerResult;
use crate::compiler::tokenizer::{Keyword, Symbol, Token, TokenBlock};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum PrimitiveType {
    I32,
    I64,
    F32,
    F64,
    Bool,
    String,
    Void,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum Type {
    Any,
    Primitive(PrimitiveType),
    Reference(Box<Type>),
    Struct(String),
}

pub fn parse_type(block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<Type> {
    match block.children.get(*curr_idx) {
        Some((Token::Symbol(Symbol::QuestionMark), _)) => {
            *curr_idx += 1;
            Ok(Type::Any)
        }
        Some((Token::Symbol(Symbol::Reference), _)) => {
            *curr_idx += 1;
            let typ = parse_type(block, curr_idx)?;
            Ok(Type::Reference(Box::new(typ)))
        }
        Some((Token::Keyword(keyword), _)) => {
            *curr_idx += 1;
            let prim = match keyword {
                Keyword::I32 => PrimitiveType::I32,
                Keyword::I64 => PrimitiveType::I64,
                Keyword::F32 => PrimitiveType::F32,
                Keyword::F64 => PrimitiveType::F64,
                Keyword::Bool => PrimitiveType::Bool,
                Keyword::String => PrimitiveType::String,
                Keyword::Void => PrimitiveType::Void,
                _ => panic!()
            };
            Ok(Type::Primitive(prim))
        }
        _ => panic!()
    }
}