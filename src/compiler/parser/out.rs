use crate::compiler::error::{merge_file_positions, CompilerError, CompilerResult, FilePosition};
use crate::compiler::parser::expression::{parse_expression, Expression};
use crate::compiler::parser::structure::StructDeclaration;
use crate::compiler::tokenizer::{tokenize_string, Constant, Keyword, Token, TokenBlock};

#[derive(Debug, Clone)]
pub struct PrintStatement {
    pub values: Vec<Expression>,
}

fn parse_format_string(structs: &Vec<StructDeclaration>, string: &str, pos: &FilePosition) -> CompilerResult<Vec<Expression>> {
    let mut res = Vec::new();
    let mut curr = String::new();
    let mut in_format = false;
    let mut format_pos = FilePosition::invalid();
    for (idx, c) in string.chars().enumerate() {
        if in_format {
            if c == '}' {
                in_format = false;

                let mut string = Vec::new();
                for (i, c) in curr.chars().enumerate() {
                    string.push((c, FilePosition {
                        first_pos: (format_pos.first_pos.0, format_pos.first_pos.1 + i + 1),
                        last_pos: (format_pos.first_pos.0, format_pos.first_pos.1 + i + 2),
                    }));
                }
                let token_block = TokenBlock { children: tokenize_string(&string)? };
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
                curr.push(c);
            }
        } else {
            if c == '{' {
                in_format = true;
                if !curr.is_empty() {
                    res.push(Expression::String(curr));
                }
                curr = String::new();
                format_pos = FilePosition {
                    first_pos: (pos.first_pos.0, pos.first_pos.1 + idx + 1),
                    last_pos: (pos.first_pos.0, pos.first_pos.1 + idx + 1)
                }
            } else {
                curr.push(c);
            }
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
                message: "Expected string after print keyword".to_string(),
                position: Some(print_pos.clone()),
            })
        }
    } else {
        Ok(None)
    }
}