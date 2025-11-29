use crate::compiler::error::{merge_file_positions, CompilerError, CompilerResult};
use crate::compiler::parser::ast::{ASTPrimitiveType, ASTType};
use crate::compiler::tokenizer::{Token, TokenBlock};

const ERR_MESSAGE: &str = "Unexpected token, expected one of: ?, &, i32, i64, f32, f64, bool, string, void, identifier";

pub fn parse_type(block: &mut TokenBlock) -> CompilerResult<ASTType> {
    match block.get() {
        (Token::QuestionMark, pos) => Ok(ASTType::Any(pos)),
        (Token::Reference, pos) => {
            let typ = parse_type(block)?;
            let pos = merge_file_positions(pos, typ.get_pos());
            Ok(ASTType::Reference(Box::new(typ), pos))
        }
        (Token::I32, pos) => Ok(ASTType::Primitive(ASTPrimitiveType::I32, pos)),
        (Token::I64, pos) => Ok(ASTType::Primitive(ASTPrimitiveType::I64, pos)),
        (Token::F32, pos) => Ok(ASTType::Primitive(ASTPrimitiveType::F32, pos)),
        (Token::F64, pos) => Ok(ASTType::Primitive(ASTPrimitiveType::F64, pos)),
        (Token::Bool, pos) => Ok(ASTType::Primitive(ASTPrimitiveType::Bool, pos)),
        (Token::String, pos) => Ok(ASTType::Primitive(ASTPrimitiveType::String, pos)),
        (Token::Void, pos) => Ok(ASTType::Primitive(ASTPrimitiveType::Void, pos)),
        (Token::Identifier(name), pos) => Ok(ASTType::Struct(name, pos)),
        (_, pos) => Err(CompilerError {
            message: ERR_MESSAGE.to_string(),
            position: Some(pos),
        })
    }
}
