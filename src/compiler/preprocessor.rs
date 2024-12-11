use crate::compiler::error::{CompilerError, CompilerResult, FilePosition};

// splits input into lines and parses indentation levels
pub fn parse_indentation(input: &str) -> CompilerResult<Vec<(i32, String)>> {
    let mut lines = Vec::new();
    for (line_idx, line) in input.lines().into_iter().enumerate() {
        let mut indent = 0;
        let leading_spaces = line.chars().take_while(|c| *c == ' ').count();
        if leading_spaces % 4 != 0 {
            return Err(CompilerError {
                message: format!("Identation must have a multiple of 4 spaces, found {} spaces", leading_spaces),
                position: FilePosition {
                    line_start: line_idx,
                    line_end: line_idx,
                    column_start: 0,
                    column_end: leading_spaces,
                }
            })
        }

        indent = (leading_spaces / 4) as i32;
        let line = line.trim().to_string();
        if !line.is_empty() {
            lines.push((indent, line));
        }
    }
    Ok(lines)
}

pub fn remove_comments(input: &str) -> String {
    let mut res = String::new();

    let mut in_comment_depth = 0;
    let mut in_single_line_comment = false;
    let mut in_string = false;

    let mut chars = input.chars().peekable();
    while let Some(c) = chars.next() {
        if in_comment_depth == 0 {
            if c == '"' {
                in_string = !in_string;
            }
            if !in_string {
                if c == '/' {
                    if let Some('/') = chars.peek() {
                        chars.next();
                        in_single_line_comment = true;
                        continue;
                    } else if let Some('*') = chars.peek() {
                        chars.next();
                        if !in_single_line_comment {
                            in_comment_depth += 1;
                        }
                        continue;
                    }
                } else if c == '\n' {
                    in_single_line_comment = false;
                }
            }
        } else {
            if c == '/' {
                if let Some('*') = chars.peek() {
                    chars.next();
                    in_comment_depth += 1;
                    continue;
                }
            } else if c == '*' {
                if let Some('/') = chars.peek() {
                    chars.next();
                    in_comment_depth -= 1;
                    continue;
                }
            }
        }
        if in_comment_depth == 0 && !in_single_line_comment {
            res.push(c);
        }
    }

    res
}