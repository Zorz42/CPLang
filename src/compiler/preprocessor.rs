use crate::compiler::error::{CompilerError, CompilerResult, FilePosition, merge_file_positions};

/*
The compiler first breaks the code into fragments.
A `Fragment` is a structure of a few types:
- String literal
- Single character
- Brace block (enclosed in `{}`)
- Bracket block (enclosed in `[]`)
- Parenthesis block (enclosed in `()`)

Preprocessor does the following in the exact order:
1. It annotates all characters with their file positions so that errors can be reported accurately.
2. It parses the input for strings and comments. It handles escape sequences within strings and edge cases such as
comments inside strings, strings inside comments, and nested comments. This is why this step is done first.
3. It parses the matching braces, brackets, and parentheses, creating a hierarchical structure of `FragmentBlock`s.
4. It processes indentation levels to determine code blocks and artificially adds brace blocks which can be deduced from indentation.
It processes raw string into a FragmentBlock which is effectively and array of Fragments with nested blocks. This is then tokenized.
 */

#[derive(Clone, Debug)]
pub enum Fragment {
    String(Vec<PosChar>, FilePosition),
    Char(PosChar),
    BraceBlock(FragmentBlock),       // {}
    BracketBlock(FragmentBlock),     // []
    ParenthesisBlock(FragmentBlock), // ()
}

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

#[derive(Clone, Debug)]
pub struct FragmentBlock {
    pub fragments: Vec<Fragment>,
    pub position: FilePosition,
}

impl Fragment {
    pub fn get_position(&self) -> FilePosition {
        match self {
            Fragment::String(_s, pos) => pos.clone(),
            Fragment::Char(pc) => pc.pos.clone(),
            Fragment::BraceBlock(b) | Fragment::BracketBlock(b) | Fragment::ParenthesisBlock(b) => b.position.clone(),
        }
    }
}

impl FragmentBlock {
    pub fn from_vec(fragments: Vec<Fragment>) -> Self {
        let mut position = FilePosition::unknown();
        for fragment in &fragments {
            let fragment_pos = fragment.get_position();
            position = merge_file_positions(&position, &fragment_pos);
        }
        Self { fragments, position }
    }
}

pub fn preprocess(input: &str) -> CompilerResult<FragmentBlock> {
    let pos_chars = add_file_positions(input);
    let fragments = parse_strings_and_comments(&pos_chars)?;
    let fragment_block = parse_blocks(&fragments, &mut 0)?;
    let fragment_block = parse_indentation(&fragment_block)?;
    let fragment_block = newlines_to_spaces(fragment_block);
    Ok(fragment_block)
}

fn add_file_positions(test: &str) -> Vec<PosChar> {
    let mut res = Vec::new();
    let mut line = 0;
    let mut column = 0;
    for c in test.chars() {
        res.push(PosChar::new(
            c,
            FilePosition {
                first_pos: (line, column),
                last_pos: (line, column + 1),
            },
        ));
        if c == '\n' {
            line += 1;
            column = 0;
        } else {
            column += 1;
        }
    }
    res
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

fn newlines_to_spaces(mut input: FragmentBlock) -> FragmentBlock {
    for i in &mut input.fragments {
        if let Fragment::Char(pos_char) = i
            && pos_char.c == '\n'
        {
            pos_char.c = ' ';
        }

        if let Fragment::BraceBlock(block) = i {
            *block = newlines_to_spaces(block.clone());
        }
        if let Fragment::BracketBlock(block) = i {
            *block = newlines_to_spaces(block.clone());
        }
        if let Fragment::ParenthesisBlock(block) = i {
            *block = newlines_to_spaces(block.clone());
        }
    }
    input
}

pub fn parse_strings_and_comments(input: &Vec<PosChar>) -> CompilerResult<Vec<Fragment>> {
    let mut res = Vec::new();

    let input = tabs_to_spaces(input);

    #[derive(PartialEq, Clone)]
    enum Location {
        String,
        SingleLineComment,
        // depth of nested comments
        MultiLineComment(i32),
        Code,
    }

    let mut location = Location::Code;
    let mut current_string = Vec::new();
    let mut multiline_comment_start_positions = Vec::new();
    let mut string_quote_position = None;

    // first split into fragments based on strings and comments
    // ignore indentation and line breaks for now
    let mut chars = input.iter().peekable();
    while let Some(pos_char) = chars.next() {
        match location.clone() {
            Location::Code => {
                if pos_char.c == '"' {
                    location = Location::String;
                    string_quote_position = Some(pos_char.pos.clone());
                    current_string = Vec::new();
                } else if pos_char.c == '/' {
                    if let Some(next_char) = chars.peek() {
                        if next_char.c == '/' {
                            location = Location::SingleLineComment;
                            chars.next();
                        } else if next_char.c == '*' {
                            location = Location::MultiLineComment(1);
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
            }
            Location::String => {
                if pos_char.c == '"' {
                    let pos = merge_file_positions(string_quote_position.as_ref().unwrap(), &pos_char.pos);
                    res.push(Fragment::String(current_string.clone(), pos));
                    location = Location::Code;
                }
                // check for \
                else if pos_char.c == '\\'
                    && let Some(next_char) = chars.peek()
                    && next_char.c == '"'
                {
                    // remove the \
                    current_string.push((*next_char).clone());
                    chars.next();
                } else {
                    current_string.push(pos_char.clone());
                }
            }
            Location::SingleLineComment => {
                if pos_char.c == '\n' {
                    location = Location::Code;
                }
            }
            Location::MultiLineComment(depth) => {
                // check for */
                if pos_char.c == '*'
                    && let Some(next_char) = chars.peek()
                    && next_char.c == '/'
                {
                    location = match depth {
                        1 => Location::Code,
                        _ => Location::MultiLineComment(depth - 1),
                    };
                    multiline_comment_start_positions.pop();
                    chars.next();
                }
                // check for /*
                else if pos_char.c == '/'
                    && let Some(next_char) = chars.peek()
                    && next_char.c == '*'
                {
                    location = Location::MultiLineComment(depth + 1);
                    multiline_comment_start_positions.push(pos_char.pos.clone());
                    chars.next();
                }
            }
        }
    }

    match &location {
        Location::String => {
            let start_pos = &string_quote_position.unwrap();
            let end_pos = &input.last().unwrap().pos;

            return Err(CompilerError {
                message: "Unclosed string literal".to_string(),
                position: Some(merge_file_positions(start_pos, end_pos)),
            });
        }
        Location::MultiLineComment(_) => {
            let start_pos = &multiline_comment_start_positions.pop().unwrap();
            let end_pos = &input.last().unwrap().pos;

            return Err(CompilerError {
                message: "Unclosed multiline comment".to_string(),
                position: Some(merge_file_positions(start_pos, end_pos)),
            });
        }
        _ => {}
    }

    Ok(res)
}

// Parses ( ), { }, [ ] blocks recursively
pub fn parse_blocks(input: &Vec<Fragment>, idx: &mut usize) -> CompilerResult<FragmentBlock> {
    let mut res = Vec::new();

    while *idx < input.len() {
        match &input[*idx] {
            Fragment::Char(PosChar { c: ')', .. }) | Fragment::Char(PosChar { c: ']', .. }) | Fragment::Char(PosChar { c: '}', .. }) => {
                return Ok(FragmentBlock::from_vec(res));
            }
            Fragment::Char(PosChar { c: '(', .. }) | Fragment::Char(PosChar { c: '[', .. }) | Fragment::Char(PosChar { c: '{', .. }) => {
                let (opening_char, opening_pos) = match &input[*idx] {
                    Fragment::Char(PosChar { c: '(', pos }) => ('(', pos),
                    Fragment::Char(PosChar { c: '[', pos }) => ('[', pos),
                    Fragment::Char(PosChar { c: '{', pos }) => ('{', pos),
                    _ => unreachable!(),
                };

                let closing_char = match opening_char {
                    '(' => ')',
                    '[' => ']',
                    '{' => '}',
                    _ => unreachable!(),
                };

                *idx += 1; // consume opening char

                let fragment_block = parse_blocks(input, idx)?;

                // expect closing char
                if *idx >= input.len() {
                    return Err(CompilerError {
                        message: format!("Unclosed {}", opening_char),
                        position: Some(opening_pos.clone()),
                    });
                }
                match &input[*idx] {
                    Fragment::Char(PosChar { c, .. }) if *c == closing_char => {
                        *idx += 1; // consume closing char
                        let fragment = match opening_char {
                            '(' => Fragment::ParenthesisBlock(fragment_block),
                            '[' => Fragment::BracketBlock(fragment_block),
                            '{' => Fragment::BraceBlock(fragment_block),
                            _ => unreachable!(),
                        };
                        res.push(fragment);
                    }
                    _ => {
                        return Err(CompilerError {
                            message: format!("Expected closing '{}'", closing_char),
                            position: None,
                        });
                    }
                }
            }
            _ => {
                res.push(input[*idx].clone());
                *idx += 1;
            }
        }
    }

    Ok(FragmentBlock::from_vec(res))
}

// recursively convert indentation levels into blocks
fn parse_indentation_block(lines: &Vec<(i32, Vec<Fragment>)>, curr_idx: &mut usize) -> CompilerResult<FragmentBlock> {
    let mut res = Vec::new();

    let curr_ident = lines[*curr_idx].0;

    while *curr_idx < lines.len() {
        let ident = lines[*curr_idx].0;

        if ident == curr_ident {
            for i in &lines[*curr_idx].1 {
                res.push(i.clone());
            }
            res.push(Fragment::Char(PosChar::new(' ', FilePosition::unknown())));
            *curr_idx += 1;
        } else if ident > curr_ident {
            let child_block = parse_indentation_block(lines, curr_idx)?;
            res.push(Fragment::BraceBlock(child_block));
        } else if ident < curr_ident {
            return Ok(FragmentBlock::from_vec(res));
        }
    }

    Ok(FragmentBlock::from_vec(res))
}

// splits input into lines and parses indentation levels
fn parse_indentation(input: &FragmentBlock) -> CompilerResult<FragmentBlock> {
    let mut lines = Vec::new();
    lines.push((0, Vec::new()));

    for i in &input.fragments {
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
                _ => panic!(),
            };
            let last_char_pos = match line[leading_spaces - 1] {
                Fragment::Char(ref pc) => &pc.pos,
                _ => panic!(),
            };

            return Err(CompilerError {
                message: format!("Indentation must have a multiple of 4 spaces, found {} spaces", leading_spaces),
                position: Some(merge_file_positions(first_char_pos, last_char_pos)),
            });
        }

        *indent = (leading_spaces / 4) as i32;
    }

    parse_indentation_block(&lines, &mut 0)
}
