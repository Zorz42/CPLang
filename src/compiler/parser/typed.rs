use crate::compiler::error::{merge_file_positions, CompilerResult};
use crate::compiler::parser::ast::{ASTPrimitiveType, ASTType};
use crate::compiler::tokenizer::{Keyword, Symbol, Token, TokenBlock};

pub fn parse_type(block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<ASTType> {
    match block.children.get(*curr_idx) {
        Some((Token::Symbol(Symbol::QuestionMark), pos)) => {
            *curr_idx += 1;
            Ok(ASTType::Any(pos.clone()))
        }
        Some((Token::Symbol(Symbol::Reference), pos)) => {
            *curr_idx += 1;
            let typ = parse_type(block, curr_idx)?;
            let pos = merge_file_positions(pos, &typ.get_pos());
            Ok(ASTType::Reference(Box::new(typ), pos))
        }
        Some((Token::Keyword(keyword), pos)) => {
            *curr_idx += 1;
            let prim = match keyword {
                Keyword::I32 => ASTPrimitiveType::I32,
                Keyword::I64 => ASTPrimitiveType::I64,
                Keyword::F32 => ASTPrimitiveType::F32,
                Keyword::F64 => ASTPrimitiveType::F64,
                Keyword::Bool => ASTPrimitiveType::Bool,
                Keyword::String => ASTPrimitiveType::String,
                Keyword::Void => ASTPrimitiveType::Void,
                _ => panic!(),
            };
            Ok(ASTType::Primitive(prim, pos.clone()))
        }
        _ => panic!(),
    }
}
