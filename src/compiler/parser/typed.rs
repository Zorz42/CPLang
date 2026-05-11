use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::parser::ast::{ASTPrimitiveType, ASTType};
use crate::compiler::parser::template::parse_template_instantiation;
use crate::compiler::tokenizer::{Token, TokenBlock};

const ERR_MESSAGE: &str = "Unexpected token, expected one of: (, ?, &, i32, i64, f32, f64, bool, string, void, identifier";

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
            (Token::I32, pos) => ASTType::Primitive(ASTPrimitiveType::I32, pos),
            (Token::I64, pos) => ASTType::Primitive(ASTPrimitiveType::I64, pos),
            (Token::F32, pos) => ASTType::Primitive(ASTPrimitiveType::F32, pos),
            (Token::F64, pos) => ASTType::Primitive(ASTPrimitiveType::F64, pos),
            (Token::Bool, pos) => ASTType::Primitive(ASTPrimitiveType::Bool, pos),
            (Token::String, pos) => ASTType::Primitive(ASTPrimitiveType::String, pos),
            (Token::Void, pos) => ASTType::Primitive(ASTPrimitiveType::Void, pos),
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
