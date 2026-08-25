use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};
use crate::compiler::parser::ast::ASTType;
use crate::compiler::parser::typed::parse_type;
use crate::compiler::tokenizer::{Token, TokenBlock};

pub fn parse_declaration_template(block: &mut TokenBlock) -> CompilerResult<Vec<(String, FilePosition)>> {
    let mut template = Vec::new();
    if let (Token::BracketBlock(_), _pos) = block.peek() {
        let Token::BracketBlock(bracket_block) = block.get().0 else { unreachable!() };

        for token in bracket_block.into_iter() {
            match token {
                (Token::Identifier(name), token_pos) => {
                    template.push((name, token_pos));
                }
                (Token::End, _) => {
                    return Err(CompilerError {
                        message: "Expected another token after this one".to_string(),
                        position: Some(block.get_last_pos()),
                    });
                }
                (_, token_pos) => {
                    return Err(CompilerError {
                        message: "Unexpected token, expected identifier".to_string(),
                        position: Some(token_pos),
                    });
                }
            }
        }
    }
    Ok(template)
}

pub fn parse_template_instantiation(block: &mut TokenBlock) -> CompilerResult<(Vec<ASTType>, FilePosition)> {
    let mut template = Vec::new();
    let mut pos = FilePosition::unknown();
    if let Token::BracketBlock(_) = block.peek().0 {
        let (Token::BracketBlock(mut block), block_pos) = block.get() else {
            unreachable!()
        };
        pos = block_pos;
        while block.has_tokens() {
            template.push(parse_type(&mut block)?);
        }
    }
    Ok((template, pos))
}
