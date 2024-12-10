#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    If,
    Else,
    While,
    For,
    Fn,
}

impl Keyword {
    fn from_str(s: &str) -> Option<Keyword> {
        match s {
            "if" => Some(Keyword::If),
            "else" => Some(Keyword::Else),
            "while" => Some(Keyword::While),
            "for" => Some(Keyword::For),
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
        return Token::Constant(Constant::String(string.to_string()));
    }

    if string == "true" || string == "false" {
        return Token::Constant(Constant::Boolean(string == "true"));
    }

    Token::Identifier(string.to_string())
}

fn tokenize_line(line: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut curr_token = String::new();

    for c in line.chars() {
        if Symbol::from_char(c).is_some() {
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

fn tokenize_block(lines: &Vec<(i32, String)>, curr_idx: &mut usize) -> TokenBlock {
    let mut block = TokenBlock {
        children: Vec::new(),
    };

    let curr_ident = lines[*curr_idx].0;

    while *curr_idx < lines.len() {
        let ident = lines[*curr_idx].0;

        if ident == curr_ident {
            let tokens = tokenize_line(&lines[*curr_idx].1);
            for i in tokens {
                block.children.push(i);
            }
            *curr_idx += 1;

        } else if ident == curr_ident + 1 {
            let child_block = tokenize_block(lines, curr_idx);
            block.children.push(Token::Block(child_block));

        } else if ident == curr_ident - 1 {
            return block;

        } else {
            panic!("Invalid indentation");
        }
    }

    block
}

// parses indentation into blocks: a block is a list of lines with the same indentation level
pub fn tokenize_blocks(lines: Vec<(i32, String)>) -> TokenBlock {
    let mut idx = 0;
    tokenize_block(&lines, &mut idx)
}
