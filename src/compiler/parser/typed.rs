use crate::compiler::error::{merge_file_positions, CompilerError, CompilerResult, FilePosition};
use crate::compiler::parser::ast::{ASTPrimitiveType, ASTType};
use crate::compiler::tokenizer::{Token, TokenBlock};

const ERR_MESSAGE: &str = "Unexpected token, expected one of: ?, &, i32, i64, f32, f64, bool, string, void, identifier";

pub fn parse_type(block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<ASTType> {
    *curr_idx += 1;
    match block.children.get(*curr_idx - 1).cloned() {
        Some((Token::QuestionMark, pos)) => Ok(ASTType::Any(pos)),
        Some((Token::Reference, pos)) => {
            let typ = parse_type(block, curr_idx)?;
            let pos = merge_file_positions(&pos, &typ.get_pos());
            Ok(ASTType::Reference(Box::new(typ), pos))
        }
        Some((Token::I32, pos)) => Ok(ASTType::Primitive(ASTPrimitiveType::I32, pos)),
        Some((Token::I64, pos)) => Ok(ASTType::Primitive(ASTPrimitiveType::I64, pos)),
        Some((Token::F32, pos)) => Ok(ASTType::Primitive(ASTPrimitiveType::F32, pos)),
        Some((Token::F64, pos)) => Ok(ASTType::Primitive(ASTPrimitiveType::F64, pos)),
        Some((Token::Bool, pos)) => Ok(ASTType::Primitive(ASTPrimitiveType::Bool, pos)),
        Some((Token::String, pos)) => Ok(ASTType::Primitive(ASTPrimitiveType::String, pos)),
        Some((Token::Void, pos)) => Ok(ASTType::Primitive(ASTPrimitiveType::Void, pos)),
        Some((Token::Identifier(name), pos)) => Ok(ASTType::Struct(name, pos)),
        _ => Err(CompilerError {
            message: ERR_MESSAGE.to_string(),
            position: Some(block.children.get(*curr_idx - 1).map(|(_x, y)| y.clone()).unwrap_or(FilePosition::unknown()).clone()),
        })
    }
}
