use crate::compiler::error::{merge_file_positions, CompilerError, CompilerResult, FilePosition};

pub fn add_file_positions(test: &str) -> Vec<(char, FilePosition)> {
    let mut res = Vec::new();
    let mut line = 0;
    let mut column = 0;
    for c in test.chars() {
        res.push((c, FilePosition {
            positions: vec![(line, column)],
        }));
        if c == '\n' {
            line += 1;
            column = 0;
        } else {
            column += 1;
        }
    }
    res
}

pub fn remove_comments(input: Vec<(char, FilePosition)>) -> CompilerResult<Vec<(char, FilePosition)>> {
    let mut res = Vec::new();

    let mut in_comment_depth = 0;
    let mut multiline_comment_positions = Vec::new();
    let mut in_single_line_comment = false;
    let mut in_string = false;

    let mut chars = input.iter().peekable();
    while let Some((c, pos)) = chars.next() {
        if *c != '\n' {
            for i in &mut multiline_comment_positions {
                *i = merge_file_positions(i, pos);
            }
        }

        if !in_string && !in_single_line_comment {
            if *c == '/' {
                // check for /*
                if let Some(('*', _)) = chars.peek() {
                    in_comment_depth += 1;
                    multiline_comment_positions.push(pos.clone());
                    continue;
                }
            } else if *c == '*' {
                // check for */
                if let Some(('/', _)) = chars.peek() {
                    let (_, pos) = chars.next().unwrap();
                    for i in &mut multiline_comment_positions {
                        *i = merge_file_positions(i, pos);
                    }
                    in_comment_depth -= 1;
                    multiline_comment_positions.pop().unwrap();
                    continue;
                }
            }
        }

        if in_comment_depth == 0 {
            if *c == '"' {
                in_string = !in_string;
            }
            if !in_string {
                if *c == '/' {
                    if let Some(('/', _)) = chars.peek() {
                        chars.next();
                        in_single_line_comment = true;
                        continue;
                    }
                } else if *c == '\n' {
                    in_single_line_comment = false;
                }
            }
        }

        if in_comment_depth == 0 && !in_single_line_comment {
            res.push((c.clone(), pos.clone()));
        }
    }

    if in_comment_depth > 0 {
        return Err(CompilerError {
            message: "Unclosed multiline comment".to_string(),
            position: multiline_comment_positions.pop().unwrap(),
        });
    }

    Ok(res)
}

// splits input into lines and parses indentation levels
pub fn parse_indentation(input: Vec<(char, FilePosition)>) -> CompilerResult<Vec<(i32, Vec<(char, FilePosition)>)>> {
    let mut lines = Vec::new();
    lines.push((0, Vec::new()));

    for (ch, pos) in input {
        if ch == '\n' {
            lines.push((0, Vec::new()));
        } else {
            lines.last_mut().unwrap().1.push((ch, pos));
        }
    }

    for (_, (indent, line)) in lines.iter_mut().enumerate() {
        let leading_spaces = line.iter().take_while(|c| c.0 == ' ').count();
        if leading_spaces % 4 != 0 {
            let mut pos = FilePosition {
                positions: Vec::new(),
            };
            for i in 0..leading_spaces {
                pos = merge_file_positions(&pos, &line[i].1);
            }

            return Err(CompilerError {
                message: format!("Identation must have a multiple of 4 spaces, found {} spaces", leading_spaces),
                position: pos,
            })
        }

        *indent = (leading_spaces / 4) as i32;
    }

    lines.retain(|(_, line)| !line.iter().all(|(c, _)| *c == ' '));

    Ok(lines)
}