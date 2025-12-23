use crate::compiler::error::{CompilerResult, FilePosition};
use crate::compiler::preprocessor::{Fragment, PosChar};

/*
Tokenizer transforms a tree of fragments (see preprocessor for what fragment is) into a tree of tokens.
It resolves all keywords and symbols and constants. The only data that is still stored as raw strings
are identifiers which are later resolved by normalizer as well since they require more context.
 */

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Identifier(String),
    Constant(Constant),
    BraceBlock(TokenBlock),
    BracketBlock(TokenBlock),
    ParenthesisBlock(TokenBlock),
    End,

    // keywords
    If,
    Else,
    While,
    For,
    Out,
    Return,
    Struct,
    InlineC,
    Fn,
    I32,
    I64,
    F32,
    F64,
    Void,
    String,
    Bool,
    Operator,

    // symbols
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
    QuestionMark,
    Pipe,
}

fn str_to_keyword(s: &str) -> Option<Token> {
    match s {
        "if" => Some(Token::If),
        "else" => Some(Token::Else),
        "while" => Some(Token::While),
        "for" => Some(Token::For),
        "out" => Some(Token::Out),
        "ret" => Some(Token::Return),
        "struct" => Some(Token::Struct),
        "inline_c" => Some(Token::InlineC),
        "fn" => Some(Token::Fn),
        "i32" => Some(Token::I32),
        "i64" => Some(Token::I64),
        "f32" => Some(Token::F32),
        "f64" => Some(Token::F64),
        "void" => Some(Token::Void),
        "string" => Some(Token::String),
        "bool" => Some(Token::Bool),
        "operator" => Some(Token::Operator),
        _ => None,
    }
}

const fn symbol_from_char(c: char) -> Option<Token> {
    match c {
        '+' => Some(Token::Plus),
        '*' => Some(Token::Star),
        '/' => Some(Token::Slash),
        '=' => Some(Token::Assign),
        '<' => Some(Token::LessThan),
        '>' => Some(Token::GreaterThan),
        '-' => Some(Token::Minus),
        '&' => Some(Token::Reference),
        '.' => Some(Token::Dot),
        ':' => Some(Token::Colon),
        '?' => Some(Token::QuestionMark),
        '|' => Some(Token::Pipe),
        _ => None,
    }
}

const fn symbol_from_two_chars(c1: char, c2: char) -> Option<Token> {
    match (c1, c2) {
        ('=', '=') => Some(Token::Equals),
        ('!', '=') => Some(Token::NotEquals),
        ('<', '=') => Some(Token::LessThanOrEqual),
        ('>', '=') => Some(Token::GreaterThanOrEqual),
        ('+', '=') => Some(Token::Increase),
        ('-', '=') => Some(Token::Decrease),
        ('+', '+') => Some(Token::Increment),
        ('-', '-') => Some(Token::Decrement),
        _ => None,
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
    tokens: Vec<(Token, FilePosition)>,
}

const LAST_TOKEN: (Token, FilePosition) = (Token::End, FilePosition::unknown());

impl TokenBlock {
    pub fn new(mut tokens: Vec<(Token, FilePosition)>) -> Self {
        tokens.reverse();
        Self { tokens }
    }

    pub fn peek(&self) -> &(Token, FilePosition) {
        self.tokens.last().unwrap_or(&LAST_TOKEN)
    }

    // peek_nth(0) == peek()
    pub fn peek_nth(&self, n: usize) -> &(Token, FilePosition) {
        if n < self.tokens.len() {
            self.tokens.get(self.tokens.len() - 1 - n).unwrap_or(&LAST_TOKEN)
        } else {
            &LAST_TOKEN
        }
    }

    pub fn get(&mut self) -> (Token, FilePosition) {
        self.tokens.pop().unwrap_or_else(|| LAST_TOKEN.clone())
    }

    pub fn has_tokens(&self) -> bool {
        self.peek().0 != Token::End
    }

    pub fn into_iter(mut self) -> Vec<(Token, FilePosition)> {
        self.tokens.reverse();
        self.tokens
    }
}

fn string_to_token(string: &str) -> Token {
    if let Some(keyword) = str_to_keyword(string) {
        return keyword;
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
        if !curr_token.is_empty() {
            tokens.push((string_to_token(curr_token), *token_pos));
            curr_token.clear();
        }
    };

    let add_to_token = |curr_token: &mut String, token_pos: &mut FilePosition, c: char, pos: FilePosition| {
        if curr_token.is_empty() {
            *token_pos = pos;
        } else {
            *token_pos = *token_pos + pos;
        }
        curr_token.push(c);
    };

    let mut iter = string.iter().peekable();
    while let Some(frag) = iter.next() {
        match frag {
            Fragment::String(s, pos) => {
                new_token(&mut tokens, &mut curr_token, &mut token_pos);
                tokens.push((Token::Constant(Constant::String(s.clone())), *pos));
            }
            Fragment::Char(pos_char) => {
                let c = pos_char.c;
                let pos = &pos_char.pos;
                let next_char = iter.peek().map_or('\0', |x| match x {
                    Fragment::Char(pc) => pc.c,
                    _ => '\0',
                });

                if c == '.' && curr_token.parse::<i32>().is_ok() {
                    // decimal point in a float
                    add_to_token(&mut curr_token, &mut token_pos, c, *pos);
                } else if let Some(symbol) = symbol_from_two_chars(c, next_char) {
                    new_token(&mut tokens, &mut curr_token, &mut token_pos);
                    tokens.push((symbol, *pos));
                    iter.next();
                } else if let Some(symbol) = symbol_from_char(c) {
                    new_token(&mut tokens, &mut curr_token, &mut token_pos);
                    tokens.push((symbol, *pos));
                } else if c == ' ' {
                    new_token(&mut tokens, &mut curr_token, &mut token_pos);
                } else {
                    add_to_token(&mut curr_token, &mut token_pos, c, *pos);
                }
            }
            Fragment::BraceBlock(block) => {
                new_token(&mut tokens, &mut curr_token, &mut token_pos);
                let token_block = tokenize_fragments(&block.fragments)?;
                tokens.push((Token::BraceBlock(token_block), block.position));
            }
            Fragment::BracketBlock(block) => {
                new_token(&mut tokens, &mut curr_token, &mut token_pos);
                let token_block = tokenize_fragments(&block.fragments)?;
                tokens.push((Token::BracketBlock(token_block), block.position));
            }
            Fragment::ParenthesisBlock(block) => {
                new_token(&mut tokens, &mut curr_token, &mut token_pos);
                let token_block = tokenize_fragments(&block.fragments)?;
                tokens.push((Token::ParenthesisBlock(token_block), block.position));
            }
        }
    }

    if !curr_token.is_empty() {
        tokens.push((string_to_token(&curr_token), token_pos));
    }

    Ok(TokenBlock::new(tokens))
}
