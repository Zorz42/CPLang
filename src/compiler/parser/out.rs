use crate::compiler::error::{merge_file_positions, CompilerError, CompilerResult, FilePosition};
use crate::compiler::parser::expression::{parse_expression, Expression};
use crate::compiler::parser::structure::StructDeclaration;
use crate::compiler::preprocessor::{parse_blocks, Fragment, PosChar};
use crate::compiler::tokenizer::{tokenize_fragments, Constant, Keyword, Token, TokenBlock};

#[derive(Debug, Clone)]
pub struct PrintStatement {
    pub values: Vec<Expression>,
}

fn parse_format_string(structs: &Vec<StructDeclaration>, string: &[PosChar], pos: &FilePosition) -> CompilerResult<Vec<Expression>> {
    let mut res = Vec::new();
    let mut curr = String::new();
    let mut in_format = false;
    let mut format_pos = FilePosition::unknown();
    for (idx, pc) in string.iter().enumerate() {
        if in_format {
            if pc.c == '}' {
                in_format = false;

                let mut string = Vec::new();
                for (i, c) in curr.chars().enumerate() {
                    string.push(Fragment::Char(PosChar::new(c, FilePosition {
                        first_pos: (format_pos.first_pos.0, format_pos.first_pos.1 + i + 1),
                        last_pos: (format_pos.first_pos.0, format_pos.first_pos.1 + i + 2),
                    })));
                }
                let fragment_block = parse_blocks(&string, &mut 0)?;
                let token_block = tokenize_fragments(&fragment_block.fragments)?;
                let mut idx2 = 0;
                let (expression, _) = parse_expression(structs, &token_block, &mut idx2)?;

                let end_pos = FilePosition {
                    first_pos: (pos.first_pos.0, pos.first_pos.1 + idx + 2),
                    last_pos: (pos.first_pos.0, pos.first_pos.1 + idx + 2)
                };
                
                if idx2 != token_block.children.len() {
                    return Err(CompilerError {
                        message: "There are multiple expressions in one format string".to_string(),
                        position: Some(merge_file_positions(&format_pos, &end_pos)),
                    })
                }

                res.push(expression);

                curr = String::new();
            } else {
                curr.push(pc.c);
            }
        } else if pc.c == '{' {
            in_format = true;
            if !curr.is_empty() {
                res.push(Expression::String(curr));
            }
            curr = String::new();
            format_pos = pc.pos.clone();
        } else {
            curr.push(pc.c);
        }
    }
    if in_format {
        let end_pos = FilePosition {
            first_pos: (pos.first_pos.0, pos.first_pos.1 + string.len() + 1),
            last_pos: (pos.first_pos.0, pos.first_pos.1 + string.len() + 1)
        };
        return Err(CompilerError {
            message: "Expected } to close format string".to_string(),
            position: Some(merge_file_positions(&format_pos, &end_pos)),
        });
    }

    if !curr.is_empty() {
        res.push(Expression::String(curr));
    }
    Ok(res)
}

pub fn parse_out_statement(structs: &Vec<StructDeclaration>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<Option<PrintStatement>> {
    let print_pos = &block.children[*curr_idx].1;
    if block.children[*curr_idx].0 == Token::Keyword(Keyword::Out) {
        *curr_idx += 1;
        let pos = &block.children[*curr_idx].1;
        match &block.children[*curr_idx].0 {
            Token::Constant(Constant::String(string)) => {
                *curr_idx += 1;
                Ok(Some(PrintStatement {
                    values: parse_format_string(structs, string, pos)?,
                }))
            },
            _ => Err(CompilerError {
                message: "Expected string after out keyword".to_string(),
                position: Some(print_pos.clone()),
            })
        }
    } else {
        Ok(None)
    }
}