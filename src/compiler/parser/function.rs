use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::normalizer::builtin_functions::is_builtin;
use crate::compiler::parser::ast::{ASTFunctionCall, ASTFunctionSignature, ASTStatement, ASTStructDeclaration};
use crate::compiler::parser::expression::parse_expression;
use crate::compiler::parser::template::parse_declaration_template;
use crate::compiler::parser::typed::{parse_type, parse_type_hint};
use crate::compiler::tokenizer::{Token, TokenBlock};

pub fn parse_function_declaration(block: &mut TokenBlock) -> CompilerResult<(ASTFunctionSignature, TokenBlock)> {
    let mut res_signature = ASTFunctionSignature {
        name: String::new(),
        args: Vec::new(),
        template: Vec::new(),
        pos: FilePosition::unknown(),
        num_template_args: 0,
    };
    let res_block;

    match block.get() {
        (Token::Identifier(name), pos) => {
            res_signature.name = name;
            res_signature.pos = res_signature.pos + pos;
        }
        (_, pos) => {
            return Err(CompilerError {
                message: "Unexpected token".to_string(),
                position: Some(pos),
            });
        }
    }

    if is_builtin(&res_signature.name) {
        return Err(CompilerError {
            message: "You cannot declare a builtin function".to_string(),
            position: Some(res_signature.pos),
        });
    }

    res_signature.template = parse_declaration_template(block)?;
    res_signature.num_template_args = res_signature.template.len();

    loop {
        let (arg, arg_pos) = match block.get() {
            (Token::BraceBlock(block), _pos) => {
                res_block = block;
                break;
            }
            (Token::Identifier(arg), pos) => {
                res_signature.pos = res_signature.pos + pos;
                (arg, pos)
            }
            (_, pos) => {
                return Err(CompilerError {
                    message: "Expected block or argument identifier after function signature".to_string(),
                    position: Some(pos),
                });
            }
        };

        let type_hint = parse_type_hint(block)?;
        res_signature.pos = res_signature.pos + type_hint.get_pos();

        res_signature.args.push((arg, type_hint, arg_pos));
    }

    Ok((res_signature, res_block))
}

// this function is called when function name is already consumed
pub fn parse_function_call(structs: &Vec<ASTStructDeclaration>, block: &mut TokenBlock) -> CompilerResult<Option<(ASTFunctionCall, FilePosition)>> {
    let (ident, mut template_block, mut call_block, pos) = if let Token::Identifier(_) = block.peek_nth(0).0
        && let Token::BracketBlock(_) = block.peek_nth(1).0
        && let Token::ParenthesisBlock(_) = block.peek_nth(2).0
    {
        // collect tokens if they match
        let (Token::Identifier(ident), pos1) = block.get() else { unreachable!() };
        let (Token::BracketBlock(template_block), pos2) = block.get() else {
            unreachable!()
        };
        let (Token::ParenthesisBlock(call_block), pos3) = block.get() else {
            unreachable!()
        };

        let pos = pos1 + pos2 + pos3;

        (ident, template_block, call_block, pos)
    } else if let Token::Identifier(_) = block.peek_nth(0).0
        && let Token::ParenthesisBlock(_) = block.peek_nth(1).0
    {
        // collect tokens if they match
        let (Token::Identifier(ident), pos1) = block.get() else { unreachable!() };
        let (Token::ParenthesisBlock(call_block), pos2) = block.get() else {
            unreachable!()
        };

        let pos = pos1 + pos2;
        let template_block = TokenBlock::new(Vec::new());

        (ident, template_block, call_block, pos)
    } else {
        return Ok(None);
    };

    let mut args = Vec::new();

    while call_block.has_tokens() {
        let expr = parse_expression(structs, &mut call_block)?;
        args.push(expr);
    }

    let mut template_args = Vec::new();

    while template_block.has_tokens() {
        let typ = parse_type(&mut template_block)?;
        template_args.push(typ);
    }

    Ok(Some((
        ASTFunctionCall {
            name: ident,
            arguments: args,
            template_arguments: template_args,
        },
        pos,
    )))
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
    let pos2 = expression.pos;
    Ok(Some(ASTStatement::Return {
        return_value: Some(expression),
        pos: pos1 + pos2,
    }))
}
