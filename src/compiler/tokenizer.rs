use crate::compiler::error::{merge_file_positions, CompilerError, CompilerResult, FilePosition};

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    If,
    Else,
    While,
    For,
    Fn,
    Print,
    Return,
}

impl Keyword {
    fn from_str(s: &str) -> Option<Keyword> {
        match s {
            "if" => Some(Keyword::If),
            "else" => Some(Keyword::Else),
            "while" => Some(Keyword::While),
            "for" => Some(Keyword::For),
            "fn" => Some(Keyword::Fn),
            "print" => Some(Keyword::Print),
            "return" => Some(Keyword::Return),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Symbol {
    Plus,
    Star,
    LeftBracket,
    RightBracket,
    Equal,
}

impl Symbol {
    fn from_char(c: char) -> Option<Symbol> {
        match c {
            '+' => Some(Symbol::Plus),
            '*' => Some(Symbol::Star),
            '(' => Some(Symbol::LeftBracket),
            ')' => Some(Symbol::RightBracket),
            '=' => Some(Symbol::Equal),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Constant {
    Integer(i32),
    Float(f32),
    String(String),
    Boolean(bool),
}

// a block is usually in an if statement, while loop, or function
// the whole program is also a block
// blocks are formed by indentation levels
#[derive(Debug, PartialEq, Clone)]
pub struct TokenBlock {
    pub(crate) children: Vec<(Token, FilePosition)>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Keyword(Keyword),
    Identifier(String),
    Symbol(Symbol),
    Constant(Constant),
    Block(TokenBlock),
}

fn string_to_token(string: &str) -> Token {
    if let Some(keyword) = Keyword::from_str(string) {
        return Token::Keyword(keyword);
    }

    if let Ok(integer) = string.parse::<i32>() {
        return Token::Constant(Constant::Integer(integer));
    }

    if let Ok(float) = string.parse::<f32>() {
        return Token::Constant(Constant::Float(float));
    }

    if string.starts_with("\"") && string.ends_with("\"") {
        let mut res = string.to_string();
        res.pop();
        res.remove(0);
        return Token::Constant(Constant::String(res));
    }

    if string == "true" || string == "false" {
        return Token::Constant(Constant::Boolean(string == "true"));
    }

    Token::Identifier(string.to_string())
}

pub fn tokenize_string(string: &Vec<(char, FilePosition)>) -> CompilerResult<Vec<(Token, FilePosition)>> {
    let mut tokens = Vec::new();
    let mut curr_token = String::new();

    let mut in_string = false;
    let mut token_pos = FilePosition::invalid();

    let mut new_token = |tokens: &mut Vec<(Token, FilePosition)>, curr_token: &mut String, token_pos: &mut FilePosition| {
        tokens.push((string_to_token(&curr_token), token_pos.clone()));
        curr_token.clear();
    };

    let mut add_to_token = |curr_token: &mut String, token_pos: &mut FilePosition, c: &char, pos: &FilePosition| {
        if curr_token.is_empty() {
            *token_pos = pos.clone();
        } else {
            *token_pos = merge_file_positions(&token_pos, pos);
        }
        curr_token.push(*c);
    };

    for (c, pos) in string {
        if in_string {
            add_to_token(&mut curr_token, &mut token_pos, c, pos);
            if *c == '"' {
                new_token(&mut tokens, &mut curr_token, &mut token_pos);
                in_string = false;
            }
        } else if *c == '"' {
            in_string = true;
            if curr_token.len() > 0 {
                new_token(&mut tokens,&mut curr_token, &mut token_pos);
            }
            add_to_token(&mut curr_token, &mut token_pos, c, pos);
        } else if Symbol::from_char(*c).is_some() {
            if curr_token.len() > 0 {
                new_token(&mut tokens, &mut curr_token, &mut token_pos);
            }
            tokens.push((Token::Symbol(Symbol::from_char(*c).unwrap()), token_pos.clone()));
        } else if *c == ' ' {
            if curr_token.len() > 0 {
                new_token(&mut tokens, &mut curr_token, &mut token_pos);
            }
        } else {
            add_to_token(&mut curr_token, &mut token_pos, c, pos);
        }
    }

    if in_string {
        return Err(CompilerError {
            message: "Expected \" to close string".to_string(),
            position: merge_file_positions(&token_pos, &string.last().unwrap().1),
        });
    }

    if curr_token.len() > 0 {
        tokens.push((string_to_token(&curr_token), token_pos.clone()));
    }

    Ok(tokens)
}

fn tokenize_block(lines: &Vec<(i32, Vec<(char, FilePosition)>)>, curr_idx: &mut usize) -> CompilerResult<TokenBlock> {
    let mut block = TokenBlock {
        children: Vec::new(),
    };

    let curr_ident = lines[*curr_idx].0;

    while *curr_idx < lines.len() {
        let ident = lines[*curr_idx].0;

        if ident == curr_ident {
            let tokens = tokenize_string(&lines[*curr_idx].1)?;
            for i in tokens {
                block.children.push(i);
            }
            *curr_idx += 1;

        } else if ident > curr_ident {
            let child_block = tokenize_block(lines, curr_idx)?;
            block.children.push((Token::Block(child_block), FilePosition::invalid()));

        } else if ident < curr_ident {
            return Ok(block);
        }
    }

    Ok(block)
}

// parses indentation into blocks: a block is a list of lines with the same indentation level
// line is (indentation, line)
pub fn tokenize_blocks(lines: Vec<(i32, Vec<(char, FilePosition)>)>) -> CompilerResult<TokenBlock> {
    tokenize_block(&lines, &mut 0)
}
