use crate::compiler::error::{CompilerError, CompilerResult};
use crate::compiler::tokenizer::{Constant, Keyword, Token, TokenBlock};

#[derive(Clone, Debug)]
pub struct InlineCStatement {
    pub code: String,
}

pub fn parse_inline_c(block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<Option<InlineCStatement>> {
    let inline_pos = &block.children[*curr_idx].1;
    if block.children[*curr_idx].0 == Token::Keyword(Keyword::InlineC) {
        *curr_idx += 1;
        match &block.children[*curr_idx].0 {
            Token::Constant(Constant::String(string)) => {
                *curr_idx += 1;
                Ok(Some(InlineCStatement {
                    code: string.clone(),
                }))
            },
            _ => Err(CompilerError {
                message: "Expected string after inline_c keyword".to_string(),
                position: Some(inline_pos.clone()),
            })
        }
    } else {
        Ok(None)
    }
}