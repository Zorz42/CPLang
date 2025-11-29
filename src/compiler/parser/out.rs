use crate::compiler::error::{merge_file_positions, CompilerError, CompilerResult, FilePosition};
use crate::compiler::parser::ast::{ASTExpression, ASTStatement, ASTStructDeclaration};
use crate::compiler::parser::expression::parse_expression;
use crate::compiler::preprocessor::{parse_blocks, Fragment, PosChar};
use crate::compiler::tokenizer::{tokenize_fragments, Constant, Token, TokenBlock};

fn parse_format_string(structs: &Vec<ASTStructDeclaration>, string: Vec<PosChar>, pos: FilePosition) -> CompilerResult<Vec<ASTExpression>> {
    let mut res = Vec::new();
    let mut curr = String::new();
    let mut in_format = false;
    let mut format_pos = FilePosition::unknown();
    let string_len = string.len();
    for (idx, pc) in string.into_iter().enumerate() {
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
                let mut token_block = tokenize_fragments(&fragment_block.fragments)?;
                let expression = parse_expression(structs, &mut token_block)?;

                let end_pos = FilePosition {
                    first_pos: (pos.first_pos.0, pos.first_pos.1 + idx + 2),
                    last_pos: (pos.first_pos.0, pos.first_pos.1 + idx + 2),
                };

                if token_block.has_tokens() {
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
                res.push(ASTExpression::String(curr, FilePosition::unknown()));
            }
            curr = String::new();
            format_pos = pc.pos.clone();
        } else {
            curr.push(pc.c);
        }
    }
    if in_format {
        let end_pos = FilePosition {
            first_pos: (pos.first_pos.0, pos.first_pos.1 + string_len + 1),
            last_pos: (pos.first_pos.0, pos.first_pos.1 + string_len + 1),
        };
        return Err(CompilerError {
            message: "Expected } to close format string".to_string(),
            position: Some(merge_file_positions(&format_pos, &end_pos)),
        });
    }

    if !curr.is_empty() {
        res.push(ASTExpression::String(curr, FilePosition::unknown()));
    }
    Ok(res)
}

pub fn parse_out_statement(structs: &Vec<ASTStructDeclaration>, block: &mut TokenBlock) -> CompilerResult<Option<ASTStatement>> {
    if block.peek().0 == Token::Out {
        let print_pos = block.get().1;
        match block.get() {
            (Token::Constant(Constant::String(string)), pos) => {
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
