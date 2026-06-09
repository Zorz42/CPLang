use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::parser::ast::{ASTType, PrimitiveType};
use crate::compiler::parser::template::parse_template_instantiation;
use crate::compiler::tokenizer::{Token, TokenBlock};

const ERR_MESSAGE: &str = "Unexpected token, expected one of: (, ?, &, i32, i64, f32, f64, bool, string, void, char, identifier";

pub fn parse_type(block: &mut TokenBlock) -> CompilerResult<ASTType> {
    let mut res = Vec::new();
    let mut pos = FilePosition::unknown();
    loop {
        pos += block.peek().1;

        res.push(match block.get() {
            (Token::QuestionMark, pos) => ASTType::Any(pos),
            (Token::Reference, pos) => {
                let typ = parse_type(block)?;
                let pos = pos + typ.get_pos();
                ASTType::Reference(Box::new(typ), pos)
            }
            (Token::I32, pos) => ASTType::Primitive(PrimitiveType::I32, pos),
            (Token::I64, pos) => ASTType::Primitive(PrimitiveType::I64, pos),
            (Token::F32, pos) => ASTType::Primitive(PrimitiveType::F32, pos),
            (Token::F64, pos) => ASTType::Primitive(PrimitiveType::F64, pos),
            (Token::Bool, pos) => ASTType::Primitive(PrimitiveType::Bool, pos),
            (Token::String, pos) => ASTType::Primitive(PrimitiveType::String, pos),
            (Token::Char, pos) => ASTType::Primitive(PrimitiveType::Char, pos),
            (Token::Void, pos) => ASTType::Primitive(PrimitiveType::Void, pos),
            (Token::Identifier(name), pos) => {
                let (template_args, template_pos) = parse_template_instantiation(block)?;
                ASTType::Identifier(name, pos + template_pos, template_args)
            }
            (Token::ParenthesisBlock(mut block), _) => parse_type(&mut block)?,
            (_, pos) => {
                return Err(CompilerError {
                    message: ERR_MESSAGE.to_string(),
                    position: Some(pos),
                });
            }
        });

        // look for tuples
        if block.peek().0 != Token::Comma {
            break;
        }
        block.get();
    }

    if res.len() == 1 {
        Ok(res.pop().unwrap())
    } else {
        Ok(ASTType::Tuple(res, pos))
    }
}

pub fn parse_type_hint(block: &mut TokenBlock) -> CompilerResult<ASTType> {
    Ok(match block.peek() {
        (Token::Colon, _pos) => {
            // type hint
            block.get();
            parse_type(block)?
        }
        _ => ASTType::Any(FilePosition::unknown()),
    })
}
