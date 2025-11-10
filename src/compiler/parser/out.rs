use crate::compiler::error::{CompilerError, CompilerResult, FilePosition, merge_file_positions};
use crate::compiler::parser::ast::{ASTExpression, ASTStatement, ASTStructDeclaration};
use crate::compiler::parser::expression::parse_expression;
use crate::compiler::preprocessor::{Fragment, PosChar, parse_blocks};
use crate::compiler::tokenizer::{Constant, Keyword, Token, TokenBlock, tokenize_fragments};

fn parse_format_string(structs: &Vec<ASTStructDeclaration>, string: &[PosChar], pos: &FilePosition) -> CompilerResult<Vec<ASTExpression>> {
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
                    string.push(Fragment::Char(PosChar::new(
                        c,
                        FilePosition {
                            first_pos: (format_pos.first_pos.0, format_pos.first_pos.1 + i + 1),
                            last_pos: (format_pos.first_pos.0, format_pos.first_pos.1 + i + 2),
                        },
                    )));
                }
                let fragment_block = parse_blocks(&string, &mut 0)?;
                let token_block = tokenize_fragments(&fragment_block.fragments)?;
                let mut idx2 = 0;
                let (expression, _) = parse_expression(structs, &token_block, &mut idx2)?;

                let end_pos = FilePosition {
                    first_pos: (pos.first_pos.0, pos.first_pos.1 + idx + 2),
                    last_pos: (pos.first_pos.0, pos.first_pos.1 + idx + 2),
                };

                if idx2 != token_block.children.len() {
                    return Err(CompilerError {
                        message: "There are multiple expressions in one format string".to_string(),
                        position: Some(merge_file_positions(&format_pos, &end_pos)),
                    });
                }

                res.push(expression);

                curr = String::new();
            } else {
                curr.push(pc.c);
            }
        } else if pc.c == '{' {
            in_format = true;
            if !curr.is_empty() {
                res.push(ASTExpression::String(curr));
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
            last_pos: (pos.first_pos.0, pos.first_pos.1 + string.len() + 1),
        };
        return Err(CompilerError {
            message: "Expected } to close format string".to_string(),
            position: Some(merge_file_positions(&format_pos, &end_pos)),
        });
    }

    if !curr.is_empty() {
        res.push(ASTExpression::String(curr));
    }
    Ok(res)
}

pub fn parse_out_statement(structs: &Vec<ASTStructDeclaration>, block: &TokenBlock, curr_idx: &mut usize) -> CompilerResult<Option<ASTStatement>> {
    let print_pos = &block.children[*curr_idx].1;
    if block.children[*curr_idx].0 == Token::Keyword(Keyword::Out) {
        *curr_idx += 1;
        let pos = &block.children[*curr_idx].1;
        match &block.children[*curr_idx].0 {
            Token::Constant(Constant::String(string)) => {
                *curr_idx += 1;
                Ok(Some(ASTStatement::Print {
                    values: parse_format_string(structs, string, pos)?,
                }))
            }
            _ => Err(CompilerError {
                message: "Expected string after out keyword".to_string(),
                position: Some(print_pos.clone()),
            }),
        }
    } else {
        Ok(None)
    }
}
