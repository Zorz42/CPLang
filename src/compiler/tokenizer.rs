use crate::compiler::error::{merge_file_positions, CompilerError, CompilerResult, FilePosition};

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    If,
    Else,
    While,
    For,
    Out,
    Return,
    Struct,
    InlineC,
    Fn,
}

impl Keyword {
    fn from_str(s: &str) -> Option<Keyword> {
        match s {
            "if" => Some(Keyword::If),
            "else" => Some(Keyword::Else),
            "while" => Some(Keyword::While),
            "for" => Some(Keyword::For),
            "out" => Some(Keyword::Out),
            "ret" => Some(Keyword::Return),
            "struct" => Some(Keyword::Struct),
            "inline_c" => Some(Keyword::InlineC),
            "fn" => Some(Keyword::Fn),
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
    Assign,
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Minus,
    Reference,
    Dot,
    Increase,
    Decrease,
    Increment,
    Decrement,
}

impl Symbol {
    fn from_char(c: char) -> Option<Symbol> {
        match c {
            '+' => Some(Symbol::Plus),
            '*' => Some(Symbol::Star),
            '(' => Some(Symbol::LeftBracket),
            ')' => Some(Symbol::RightBracket),
            '=' => Some(Symbol::Assign),
            '<' => Some(Symbol::LessThan),
            '>' => Some(Symbol::GreaterThan),
            '-' => Some(Symbol::Minus),
            '&' => Some(Symbol::Reference),
            '.' => Some(Symbol::Dot),
            _ => None,
        }
    }

    fn from_two_chars(c1: char, c2: char) -> Option<Symbol> {
        match (c1, c2) {
            ('=', '=') => Some(Symbol::Equals),
            ('!', '=') => Some(Symbol::NotEquals),
            ('<', '=') => Some(Symbol::LessThanOrEqual),
            ('>', '=') => Some(Symbol::GreaterThanOrEqual),
            ('+', '=') => Some(Symbol::Increase),
            ('-', '=') => Some(Symbol::Decrease),
            ('+', '+') => Some(Symbol::Increment),
            ('-', '-') => Some(Symbol::Decrement),
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

    let new_token = |tokens: &mut Vec<(Token, FilePosition)>, curr_token: &mut String, token_pos: &mut FilePosition| {
        tokens.push((string_to_token(curr_token), token_pos.clone()));
        curr_token.clear();
    };

    let add_to_token = |curr_token: &mut String, token_pos: &mut FilePosition, c: &char, pos: &FilePosition| {
        if curr_token.is_empty() {
            *token_pos = pos.clone();
        } else {
            *token_pos = merge_file_positions(token_pos, pos);
        }
        curr_token.push(*c);
    };

    let mut iter = string.iter().peekable();
    while let Some((c, pos)) = iter.next() {
        if in_string {
            if *c == '\\' && iter.peek().map(|x| x.0) == Some('"') {
                iter.next();
                add_to_token(&mut curr_token, &mut token_pos, &'"', pos);
            } else {
                add_to_token(&mut curr_token, &mut token_pos, c, pos);
                if *c == '"' {
                    new_token(&mut tokens, &mut curr_token, &mut token_pos);
                    in_string = false;
                }
            }
        } else if *c == '"' {
            in_string = true;
            if !curr_token.is_empty() {
                new_token(&mut tokens,&mut curr_token, &mut token_pos);
            }
            add_to_token(&mut curr_token, &mut token_pos, c, pos);
        } else if *c == '.' && curr_token.parse::<i32>().is_ok() {
            add_to_token(&mut curr_token, &mut token_pos, c, pos);
        } else if let Some(symbol) = Symbol::from_two_chars(*c, iter.peek().map(|x| x.0).unwrap_or(' ')) {
            if !curr_token.is_empty() {
                new_token(&mut tokens, &mut curr_token, &mut token_pos);
            }
            tokens.push((Token::Symbol(symbol), pos.clone()));
            iter.next();
        } else if let Some(symbol) = Symbol::from_char(*c) {
            if !curr_token.is_empty() {
                new_token(&mut tokens, &mut curr_token, &mut token_pos);
            }
            tokens.push((Token::Symbol(symbol), pos.clone()));
        } else if *c == ' ' {
            if !curr_token.is_empty() {
                new_token(&mut tokens, &mut curr_token, &mut token_pos);
            }
        } else {
            add_to_token(&mut curr_token, &mut token_pos, c, pos);
        }
    }

    if in_string {
        return Err(CompilerError {
            message: "Expected \" to close string".to_string(),
            position: Some(merge_file_positions(&token_pos, &string.last().unwrap().1)),
        });
    }

    if !curr_token.is_empty() {
        tokens.push((string_to_token(&curr_token), token_pos.clone()));
    }

    Ok(tokens)
}

fn get_block_position(block: &TokenBlock) -> FilePosition {
    let mut result = block.children[0].1.clone();

    for (_, pos) in &block.children {
        result = merge_file_positions(&result, pos);
    }

    result
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
            let pos = get_block_position(&child_block);
            block.children.push((Token::Block(child_block), pos));

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
