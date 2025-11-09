use crate::compiler::error::CompilerResult;
use crate::compiler::parser::ast::{ASTPrimitiveType, ASTType};
use crate::compiler::tokenizer::{Keyword, Symbol, Token, TokenBlock};

pub fn parse_type(block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<ASTType> {
    match block.children.get(*curr_idx) {
        Some((Token::Symbol(Symbol::QuestionMark), _)) => {
            *curr_idx += 1;
            Ok(ASTType::Any)
        }
        Some((Token::Symbol(Symbol::Reference), _)) => {
            *curr_idx += 1;
            let typ = parse_type(block, curr_idx)?;
            Ok(ASTType::Reference(Box::new(typ)))
        }
        Some((Token::Keyword(keyword), _)) => {
            *curr_idx += 1;
            let prim = match keyword {
                Keyword::I32 => ASTPrimitiveType::I32,
                Keyword::I64 => ASTPrimitiveType::I64,
                Keyword::F32 => ASTPrimitiveType::F32,
                Keyword::F64 => ASTPrimitiveType::F64,
                Keyword::Bool => ASTPrimitiveType::Bool,
                Keyword::String => ASTPrimitiveType::String,
                Keyword::Void => ASTPrimitiveType::Void,
                _ => panic!()
            };
            Ok(ASTType::Primitive(prim))
        }
        _ => panic!()
    }
}