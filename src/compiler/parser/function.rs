use crate::compiler::error::{merge_file_positions, CompilerError, CompilerResult, FilePosition};
use crate::compiler::parser::ast::{ASTFunctionSignature, ASTStatement, ASTStructDeclaration, ASTType};
use crate::compiler::parser::expression::parse_expression;
use crate::compiler::parser::typed::parse_type;
use crate::compiler::tokenizer::{Token, TokenBlock};

pub fn parse_function_declaration(block: &mut TokenBlock) -> CompilerResult<(ASTFunctionSignature, TokenBlock)> {
    let mut res_signature = ASTFunctionSignature {
        name: String::new(),
        args: Vec::new(),
        template: Vec::new(),
        pos: FilePosition::unknown(),
    };
    let res_block;

    match block.get() {
        (Token::Identifier(name), pos) => {
            res_signature.name = name.clone();
            res_signature.pos = merge_file_positions(res_signature.pos, pos);
        }
        (_, pos) => {
            return Err(CompilerError {
                message: "Unexpected token".to_string(),
                position: Some(pos),
            });
        }
    }

    // check for template declaration
    if let (Token::BracketBlock(_), _pos) = block.peek() {
        let Token::BracketBlock(bracket_block) = block.get().0 else { unreachable!() };

        for (token, token_pos) in bracket_block.into_iter() {
            if let Token::Identifier(name) = token {
                res_signature.template.push((name, token_pos));
            } else {
                return Err(CompilerError {
                    message: "Unexpected token, expected identifier".to_string(),
                    position: Some(token_pos),
                });
            }
        }
    }

    loop {
        let (arg, arg_pos) = match block.get() {
            (Token::BraceBlock(block), _pos) => {
                res_block = block;
                break;
            }
            (Token::Identifier(arg), pos) => {
                res_signature.pos = merge_file_positions(res_signature.pos, pos.clone());
                (arg, pos)
            }
            (_, pos) => {
                return Err(CompilerError {
                    message: "Expected block or argument identifier after function signature".to_string(),
                    position: Some(pos),
                });
            }
        };

        let type_hint = match block.peek() {
            (Token::Colon, _pos) => {
                // type hint
                let pos = block.get().1;
                res_signature.pos = merge_file_positions(res_signature.pos, pos);
                parse_type(block)?
            }
            _ => ASTType::Any(arg_pos.clone()),
        };

        res_signature.args.push((arg, type_hint, arg_pos));
    }

    Ok((res_signature, res_block))
}

pub fn parse_return_statement(structs: &Vec<ASTStructDeclaration>, block: &mut TokenBlock) -> CompilerResult<Option<ASTStatement>> {
    if block.peek().0 != Token::Return {
        return Ok(None);
    }
    let pos1 = block.get().1;

    if !block.has_tokens() {
        return Ok(Some(ASTStatement::Return { return_value: None, pos: pos1 }));
    }

    let expression = parse_expression(structs, block)?;
    let pos2 = expression.get_pos();
    Ok(Some(ASTStatement::Return {
        return_value: Some(expression),
        pos: merge_file_positions(pos1, pos2),
    }))
}
