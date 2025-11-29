use crate::compiler::error::{merge_file_positions, CompilerError, CompilerResult, FilePosition};
use crate::compiler::parser::ast::{ASTExpression, ASTStructDeclaration, ASTType};
use crate::compiler::parser::block::parse_block;
use crate::compiler::parser::expression::parse_expression;
use crate::compiler::parser::function::parse_function_declaration;
use crate::compiler::parser::typed::parse_type;
use crate::compiler::tokenizer::{Token, TokenBlock};
use std::collections::HashMap;

pub fn parse_struct_declaration(block: &mut TokenBlock) -> CompilerResult<Option<ASTStructDeclaration>> {
    if Token::Struct != block.peek().0 {
        return Ok(None);
    }

    // skip struct token
    block.get();

    let name = match block.get() {
        (Token::Identifier(name), _) => name.clone(),
        (_, pos) => {
            return Err(CompilerError {
                message: "Expected struct name after struct keyword".to_owned(),
                position: Some(pos),
            });
        }
    };

    let mut block = match block.get() {
        (Token::BraceBlock(block), _) => block.clone(),
        (_, pos) => {
            return Err(CompilerError {
                message: "Expected block after struct name".to_owned(),
                position: Some(pos),
            });
        }
    };

    // parse struct body

    let mut fields = Vec::new();
    let mut methods = Vec::new();

    while block.has_tokens() {
        match block.get() {
            (Token::Identifier(name), _) => {
                let type_hint = if Token::Colon == block.peek().0 {
                    block.get();
                    parse_type(&mut block)?
                } else {
                    ASTType::Any(FilePosition::unknown())
                };

                fields.push((name.clone(), type_hint));
            }
            (Token::Fn, _) => {
                let (signature, block) = parse_function_declaration(&mut block)?;
                let block = parse_block(&Vec::new(), block)?;
                methods.push((signature, block));
            }
            (_, pos) => {
                return Err(CompilerError {
                    message: "Expected field name of fn keyword".to_owned(),
                    position: Some(pos),
                });
            }
        }
    }

    Ok(Some(ASTStructDeclaration { name, fields, methods }))
}

pub fn parse_struct_instantiation(structs: &Vec<ASTStructDeclaration>, block: &mut TokenBlock, struct_declaration: &ASTStructDeclaration, mut pos: FilePosition, identifier: String) -> CompilerResult<ASTExpression> {
    let mut fields = HashMap::new();
    let mut fields_left = struct_declaration.fields.len();

    while fields_left > 0 {
        let (field_name, field_pos) = match block.get() {
            (Token::Identifier(ident), pos) => {
                if !struct_declaration.fields.iter().any(|(name, _type_hint)| name == &ident) {
                    return Err(CompilerError {
                        message: format!("Field with name {ident} not found"),
                        position: Some(pos),
                    });
                }

                (ident, pos)
            }
            (_, pos) => {
                return Err(CompilerError {
                    message: "Expected struct field identifier".to_owned(),
                    position: Some(pos),
                });
            }
        };

        let expr = parse_expression(structs, block)?;
        let expr_pos = expr.get_pos();

        if fields.contains_key(&field_name) {
            return Err(CompilerError {
                message: format!("Field {} assigned twice.", field_name),
                position: Some(field_pos),
            });
        }

        fields.insert(field_name, expr);
        pos = merge_file_positions(pos, expr_pos);
        fields_left -= 1;
    }

    let mut fields_res = Vec::new();
    for (field_name, _field_type) in struct_declaration.fields.iter() {
        fields_res.push(fields[field_name].clone());
    }

    Ok(ASTExpression::StructInitialization {
        name: identifier,
        fields: fields_res,
        pos,
    })
}
