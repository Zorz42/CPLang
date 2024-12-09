use crate::compiler::error::FilePosition;

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
    pub(crate) children: Vec<Token>,
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

pub fn tokenize_string(string: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut curr_token = String::new();

    let mut in_string = false;

    for c in string.chars() {
        if in_string {
            curr_token.push(c);
            if c == '"' {
                tokens.push(string_to_token(&curr_token));
                curr_token.clear();
                in_string = false;
            }
        } else if c == '"' {
            in_string = true;
            if curr_token.len() > 0 {
                tokens.push(string_to_token(&curr_token));
                curr_token.clear();
            }
            curr_token.push(c);
        } else if Symbol::from_char(c).is_some() {
            if curr_token.len() > 0 {
                tokens.push(string_to_token(&curr_token));
                curr_token.clear();
            }
            tokens.push(Token::Symbol(Symbol::from_char(c).unwrap()));
        } else if c == ' ' {
            if curr_token.len() > 0 {
                tokens.push(string_to_token(&curr_token));
                curr_token.clear();
            }
        } else {
            curr_token.push(c);
        }
    }

    if curr_token.len() > 0 {
        tokens.push(string_to_token(&curr_token));
    }

    tokens
}

fn tokenize_block(lines: &Vec<(i32, Vec<(char, FilePosition)>)>, curr_idx: &mut usize) -> TokenBlock {
    let mut block = TokenBlock {
        children: Vec::new(),
    };

    let curr_ident = lines[*curr_idx].0;

    while *curr_idx < lines.len() {
        let ident = lines[*curr_idx].0;

        if ident == curr_ident {
            let string = lines[*curr_idx].1.iter().map(|x| x.0).collect::<String>();

            let tokens = tokenize_string(&string);
            for i in tokens {
                block.children.push(i);
            }
            *curr_idx += 1;

        } else if ident > curr_ident {
            let child_block = tokenize_block(lines, curr_idx);
            block.children.push(Token::Block(child_block));

        } else if ident < curr_ident {
            return block;
        }
    }

    block
}

// parses indentation into blocks: a block is a list of lines with the same indentation level
// line is (indentation, line)
pub fn tokenize_blocks(lines: Vec<(i32, Vec<(char, FilePosition)>)>) -> TokenBlock {
    tokenize_block(&lines, &mut 0)
}
