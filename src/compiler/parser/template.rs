use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::parser::ast::ASTType;
use crate::compiler::parser::typed::parse_type;
use crate::compiler::tokenizer::{Token, TokenBlock};

pub fn parse_declaration_template(block: &mut TokenBlock) -> CompilerResult<Vec<(String, FilePosition)>> {
    let mut template = Vec::new();
    if let (Token::BracketBlock(_), _pos) = block.peek() {
        let Token::BracketBlock(bracket_block) = block.get().0 else { unreachable!() };

        for (token, token_pos) in bracket_block.into_iter() {
            if let Token::Identifier(name) = token {
                template.push((name, token_pos));
            } else {
                return Err(CompilerError {
                    message: "Unexpected token, expected identifier".to_string(),
                    position: Some(token_pos),
                });
            }
        }
    }
    Ok(template)
}

pub fn parse_template_instantiation(block: &mut TokenBlock) -> CompilerResult<Vec<ASTType>> {
    let mut template = Vec::new();
    if let Token::BracketBlock(_) = block.peek().0 {
        let Token::BracketBlock(mut block) = block.get().0 else { unreachable!() };
        while block.has_tokens() {
            template.push(parse_type(&mut block)?);
        }
    }
    Ok(template)
}
