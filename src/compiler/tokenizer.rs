use crate::compiler::error::{merge_file_positions, CompilerResult, FilePosition};
use crate::compiler::preprocessor::{Fragment, PosChar};

/*
Tokenizer transforms a tree of fragments (see preprocessor for what fragment is) into a tree of tokens.
It resolves all keywords and symbols and constants. The only data that is still stored as raw strings
are identifiers which are later resolved by normalizer as well since they require more context.
 */

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
    Colon,
}

impl Symbol {
    fn from_char(c: char) -> Option<Symbol> {
        match c {
            '+' => Some(Symbol::Plus),
            '*' => Some(Symbol::Star),
            '/' => Some(Symbol::Slash),
            '=' => Some(Symbol::Assign),
            '<' => Some(Symbol::LessThan),
            '>' => Some(Symbol::GreaterThan),
            '-' => Some(Symbol::Minus),
            '&' => Some(Symbol::Reference),
            '.' => Some(Symbol::Dot),
            ':' => Some(Symbol::Colon),
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
    pub pos: FilePosition,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Keyword(Keyword),
    Identifier(String),
    Symbol(Symbol),
    Constant(Constant),
    BraceBlock(TokenBlock),
    BracketBlock(TokenBlock),
    ParenthesisBlock(TokenBlock),
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

pub fn tokenize_fragments(string: &[Fragment]) -> CompilerResult<TokenBlock> {
    let mut tokens = Vec::new();
    let mut curr_token = String::new();
    let mut token_pos = FilePosition::unknown();

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
            Fragment::String(s, pos) => {
                if !curr_token.is_empty() {
                    new_token(&mut tokens, &mut curr_token, &mut token_pos);
                }
                tokens.push((Token::Constant(Constant::String(s.clone())), pos.clone()));
            },
            Fragment::Char(pos_char) => {
                let c = pos_char.c;
                let pos = &pos_char.pos;
                let next_char = iter.peek().map(|x| match x {
                    Fragment::Char(pc) => pc.c,
                    _ => '\0',
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
            Fragment::BraceBlock(block) => {
                if !curr_token.is_empty() {
                    new_token(&mut tokens, &mut curr_token, &mut token_pos);
                }
                let token_block = tokenize_fragments(&block.fragments)?;
                tokens.push((Token::BraceBlock(token_block), block.position.clone()));
            }
            Fragment::BracketBlock(block) => {
                if !curr_token.is_empty() {
                    new_token(&mut tokens, &mut curr_token, &mut token_pos);
                }
                let token_block = tokenize_fragments(&block.fragments)?;
                tokens.push((Token::BracketBlock(token_block), block.position.clone()));
            }
            Fragment::ParenthesisBlock(block) => {
                if !curr_token.is_empty() {
                    new_token(&mut tokens, &mut curr_token, &mut token_pos);
                }
                let token_block = tokenize_fragments(&block.fragments)?;
                tokens.push((Token::ParenthesisBlock(token_block), block.position.clone()));
            }
        }
    }

    if !curr_token.is_empty() {
        tokens.push((string_to_token(&curr_token), token_pos.clone()));
    }

    Ok(TokenBlock {
        children: tokens,
        pos: FilePosition::unknown(),
    })
}
