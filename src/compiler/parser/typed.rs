use crate::compiler::error::{merge_file_positions, CompilerError, CompilerResult, FilePosition};
use crate::compiler::parser::ast::{ASTPrimitiveType, ASTType};
use crate::compiler::tokenizer::{Keyword, Symbol, Token, TokenBlock};

const ERR_MESSAGE: &str = "Unexpected token, expected one of: ?, &, i32, i64, f32, f64, bool, string, void, identifier";

pub fn parse_type(block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<ASTType> {
    match block.children.get(*curr_idx).cloned() {
        Some((Token::Symbol(Symbol::QuestionMark), pos)) => {
            *curr_idx += 1;
            Ok(ASTType::Any(pos))
        }
        Some((Token::Symbol(Symbol::Reference), pos)) => {
            *curr_idx += 1;
            let typ = parse_type(block, curr_idx)?;
            let pos = merge_file_positions(&pos, &typ.get_pos());
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
                _ => return Err(CompilerError {
                    message: ERR_MESSAGE.to_string(),
                    position: Some(pos),
                })
            };
            Ok(ASTType::Primitive(prim, pos))
        }
        Some((Token::Identifier(name), pos)) => {
            *curr_idx += 1;
            Ok(ASTType::Struct(name, pos))
        }
        _ => Err(CompilerError {
            message: ERR_MESSAGE.to_string(),
            position: Some(block.children.get(*curr_idx).map(|(_x, y)| y.clone()).unwrap_or(FilePosition::unknown()).clone()),
        })
    }
}
