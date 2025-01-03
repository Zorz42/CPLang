use crate::compiler::error::{merge_file_positions, CompilerError, CompilerResult, FilePosition};

pub fn add_file_positions(test: &str) -> Vec<(char, FilePosition)> {
    let mut res = Vec::new();
    let mut line = 0;
    let mut column = 0;
    for c in test.chars() {
        res.push((c, FilePosition {
            first_pos: (line, column),
            last_pos: (line, column + 1),
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
        if !in_string && !in_single_line_comment {
            if *c == '/' {
                // check for /*
                if let Some(('*', _)) = chars.peek() {
                    in_comment_depth += 1;
                    multiline_comment_positions.push(pos.clone());
                    if in_comment_depth == 1 {
                        res.push((' ', pos.clone()));
                    }
                    continue;
                }
            } else if *c == '*' {
                // check for */
                if let Some(('/', _)) = chars.peek() {
                    chars.next().unwrap();
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
            res.push((*c, pos.clone()));
        }
    }

    if in_comment_depth > 0 {
        let position = merge_file_positions(&multiline_comment_positions.pop().unwrap(), &input.last().unwrap().1);

        return Err(CompilerError {
            message: "Unclosed multiline comment".to_string(),
            position: Some(position),
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

    lines.retain(|(_, line)| !line.iter().all(|(c, _)| *c == ' '));

    for (indent, line) in &mut lines {
        let leading_spaces = line.iter().take_while(|c| c.0 == ' ').count();
        if leading_spaces % 4 != 0 {
            let pos = FilePosition {
                first_pos: (line[0].1.first_pos.0, 0),
                last_pos: (line[0].1.first_pos.0, leading_spaces),
            };

            return Err(CompilerError {
                message: format!("Identation must have a multiple of 4 spaces, found {} spaces", leading_spaces),
                position: Some(pos),
            })
        }

        *indent = (leading_spaces / 4) as i32;
    }

    Ok(lines)
}