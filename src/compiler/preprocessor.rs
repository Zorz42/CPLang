use crate::compiler::error::{merge_file_positions, CompilerError, CompilerResult, FilePosition};

// A character with its position in the file
#[derive(Clone, Debug, PartialEq)]
pub struct PosChar {
    pub(crate) c: char,
    pub(crate) pos: FilePosition,
}

impl PosChar {
    pub fn new(c: char, pos: FilePosition) -> Self {
        Self { c, pos }
    }
}

pub fn add_file_positions(test: &str) -> Vec<PosChar> {
    let mut res = Vec::new();
    let mut line = 0;
    let mut column = 0;
    for c in test.chars() {
        res.push(PosChar::new(c, FilePosition {
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

// The compiler first takes all the strings, because string
// literals can contain comment-like sequences. Comments also contain
// quotes. It breaks it into fragments. A `Fragment` is a structure of
// two types: either a string literal or a char (of code).
// Strings also contain quotes so that their whole location can be tracked.
#[derive(Clone, Debug)]
pub enum Fragment {
    String(Vec<PosChar>),
    Char(PosChar),
}

fn tabs_to_spaces(input: &Vec<PosChar>) -> Vec<PosChar> {
    let mut res = Vec::new();
    for pos_char in input {
        if pos_char.c == '\t' {
            for _ in 0..4 {
                res.push(PosChar::new(' ', pos_char.pos.clone()));
            }
        } else {
            res.push(pos_char.clone());
        }
    }
    res
}

pub fn parse_strings_and_comments(input: &Vec<PosChar>) -> CompilerResult<Vec<Fragment>> {
    let mut res = Vec::new();

    let input = tabs_to_spaces(input);

    #[derive(PartialEq, Clone)]
    enum Location {
        InString,
        InSingleLineComment,
        // depth of nested comments
        InMultiLineComment(i32),
        InCode,
    }

    let mut location = Location::InCode;
    let mut current_string = Vec::new();
    let mut multiline_comment_start_positions = Vec::new();
    let mut string_quote_position = None;

    // first split into fragments based on strings and comments
    // ignore indentation and line breaks for now
    let mut chars = input.iter().peekable();
    while let Some(pos_char) = chars.next() {
        match location.clone() {
            Location::InCode => {
                if pos_char.c == '"' {
                    location = Location::InString;
                    string_quote_position = Some(pos_char.pos.clone());
                    current_string = vec![pos_char.clone()];
                } else if pos_char.c == '/' {
                    if let Some(next_char) = chars.peek() {
                        if next_char.c == '/' {
                            location = Location::InSingleLineComment;
                            chars.next();
                        } else if next_char.c == '*' {
                            location = Location::InMultiLineComment(1);
                            multiline_comment_start_positions.push(pos_char.pos.clone());
                            chars.next();
                            res.push(Fragment::Char(PosChar::new(' ', pos_char.pos.clone())));
                        } else {
                            res.push(Fragment::Char(pos_char.clone()));
                        }
                    } else {
                        res.push(Fragment::Char(pos_char.clone()));
                    }
                } else {
                    res.push(Fragment::Char(pos_char.clone()));
                }
            },
            Location::InString => {
                current_string.push(pos_char.clone());
                if pos_char.c == '"' {
                    res.push(Fragment::String(current_string.clone()));
                    location = Location::InCode;
                }
                // check for \
                else if pos_char.c == '\\' {
                    if let Some(next_char) = chars.peek() {
                        if next_char.c == '"' {
                            // remove the \
                            current_string.pop();
                            current_string.push((*next_char).clone());
                            chars.next();
                            continue;
                        }
                    }
                }
            },
            Location::InSingleLineComment => {
                if pos_char.c == '\n' {
                    location = Location::InCode;
                }
            },
            Location::InMultiLineComment(depth) => {
                if pos_char.c == '*' {
                    if let Some(next_char) = chars.peek() {
                        if next_char.c == '/' {
                            location = match depth {
                                1 => Location::InCode,
                                _ => Location::InMultiLineComment(depth - 1),
                            };
                            multiline_comment_start_positions.pop();
                            chars.next();
                        }
                    }
                }
                if pos_char.c == '/' {
                    if let Some(next_char) = chars.peek() {
                        if next_char.c == '*' {
                            location = Location::InMultiLineComment(depth + 1);
                            multiline_comment_start_positions.push(pos_char.pos.clone());
                            chars.next();
                        }
                    }
                }
            },
        }
    }

    match &location {
        Location::InString => {
            let start_pos = &string_quote_position.unwrap();
            let end_pos = &input.last().unwrap().pos;

            return Err(CompilerError {
                message: "Unclosed string literal".to_string(),
                position: Some(merge_file_positions(start_pos, end_pos)),
            });
        },
        Location::InMultiLineComment(_) => {
            let start_pos = &multiline_comment_start_positions.pop().unwrap();
            let end_pos = &input.last().unwrap().pos;

            return Err(CompilerError {
                message: "Unclosed multiline comment".to_string(),
                position: Some(merge_file_positions(start_pos, end_pos)),
            });
        }
        _ => {},
    }

    Ok(res)
}

// splits input into lines and parses indentation levels
pub fn parse_indentation(input: &Vec<Fragment>) -> CompilerResult<Vec<(i32, Vec<Fragment>)>> {
    let mut lines = Vec::new();
    lines.push((0, Vec::new()));

    for i in input {
        if matches!(i, Fragment::Char(PosChar { c: '\n', pos: _ })) {
            lines.push((0, Vec::new()));
        } else {
            lines.last_mut().unwrap().1.push(i.clone());
        }
    }

    // remove lines with only spaces
    lines.retain(|(_, line)| !line.iter().all(|i| matches!(i, Fragment::Char(PosChar { c: ' ', pos: _ }))));

    for (indent, line) in &mut lines {
        let leading_spaces = line.iter().take_while(|i| matches!(i, Fragment::Char(PosChar { c: ' ', pos: _ }))).count();
        if leading_spaces % 4 != 0 {
            let first_char_pos = match line[0] {
                Fragment::Char(ref pc) => &pc.pos,
                Fragment::String(_) => panic!()
            };
            let last_char_pos = match line[leading_spaces-1] {
                Fragment::Char(ref pc) => &pc.pos,
                Fragment::String(_) => panic!()
            };

            return Err(CompilerError {
                message: format!("Indentation must have a multiple of 4 spaces, found {} spaces", leading_spaces),
                position: Some(merge_file_positions(first_char_pos, last_char_pos)),
            })
        }

        *indent = (leading_spaces / 4) as i32;
    }

    Ok(lines)
}