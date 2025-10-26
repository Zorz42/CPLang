use crate::compiler::error::{merge_file_positions, CompilerResult, FilePosition};
use crate::compiler::preprocessor::{Fragment, PosChar};

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
    Slash,
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
            '/' => Some(Symbol::Slash),
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
    String(Vec<PosChar>),
    Boolean(bool),
}

// a block usually represents contents  an if statement, while loop, or function
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

    if string == "true" || string == "false" {
        return Token::Constant(Constant::Boolean(string == "true"));
    }

    Token::Identifier(string.to_string())
}

pub fn tokenize_fragments(string: &Vec<Fragment>) -> CompilerResult<Vec<(Token, FilePosition)>> {
    let mut tokens = Vec::new();
    let mut curr_token = String::new();
    let mut token_pos = FilePosition::invalid();

    let new_token = |tokens: &mut Vec<(Token, FilePosition)>, curr_token: &mut String, token_pos: &mut FilePosition| {
        tokens.push((string_to_token(curr_token), token_pos.clone()));
        curr_token.clear();
    };

    let add_to_token = |curr_token: &mut String, token_pos: &mut FilePosition, c: char, pos: &FilePosition| {
        if curr_token.is_empty() {
            *token_pos = pos.clone();
        } else {
            *token_pos = merge_file_positions(token_pos, pos);
        }
        curr_token.push(c);
    };

    let mut iter = string.iter().peekable();
    while let Some(frag) = iter.next() {
        match frag {
            Fragment::String(s) => {
                if !curr_token.is_empty() {
                    new_token(&mut tokens, &mut curr_token, &mut token_pos);
                }
                let mut str_content = Vec::new();
                let mut str_pos = s[0].pos.clone();
                for (i, pos_char) in s.iter().enumerate() {
                    str_pos = merge_file_positions(&str_pos, &pos_char.pos);
                    
                    if i != 0 && i != s.len() - 1 {
                        // skip the quotes
                        str_content.push(pos_char.clone());
                    }
                }
                tokens.push((Token::Constant(Constant::String(str_content)), str_pos));
            },
            Fragment::Char(pos_char) => {
                let c = pos_char.c;
                let pos = &pos_char.pos;
                let next_char = iter.peek().map(|x| match x {
                    Fragment::Char(pc) => pc.c,
                    Fragment::String(_) => '\0',
                }).unwrap_or('\0');

                if c == '.' && curr_token.parse::<i32>().is_ok() {
                    // decimal point in a float
                    add_to_token(&mut curr_token, &mut token_pos, c, pos);
                } else if let Some(symbol) = Symbol::from_two_chars(c, next_char) {
                    if !curr_token.is_empty() {
                        new_token(&mut tokens, &mut curr_token, &mut token_pos);
                    }
                    tokens.push((Token::Symbol(symbol), pos.clone()));
                    iter.next();
                } else if let Some(symbol) = Symbol::from_char(c) {
                    if !curr_token.is_empty() {
                        new_token(&mut tokens, &mut curr_token, &mut token_pos);
                    }
                    tokens.push((Token::Symbol(symbol), pos.clone()));
                } else if c == ' ' {
                    if !curr_token.is_empty() {
                        new_token(&mut tokens, &mut curr_token, &mut token_pos);
                    }
                } else {
                    add_to_token(&mut curr_token, &mut token_pos, c, pos);
                }
            }
        }
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

fn tokenize_into_block(lines: &Vec<(i32, Vec<Fragment>)>, curr_idx: &mut usize) -> CompilerResult<TokenBlock> {
    let mut block = TokenBlock {
        children: Vec::new(),
    };

    let curr_ident = lines[*curr_idx].0;

    while *curr_idx < lines.len() {
        let ident = lines[*curr_idx].0;

        if ident == curr_ident {
            let tokens = tokenize_fragments(&lines[*curr_idx].1)?;
            for i in tokens {
                block.children.push(i);
            }
            *curr_idx += 1;

        } else if ident > curr_ident {
            let child_block = tokenize_into_block(lines, curr_idx)?;
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
pub fn tokenize_lines(lines: &Vec<(i32, Vec<Fragment>)>) -> CompilerResult<TokenBlock> {
    tokenize_into_block(lines, &mut 0)
}
