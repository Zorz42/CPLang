use crate::compiler::error::{CompilerResult, FilePosition};
use crate::compiler::preprocessor::{Fragment, PosChar};

/*
Tokenizer transforms a tree of fragments (see preprocessor for what fragment is) into a tree of tokens.
It resolves all keywords and symbols and constants. The only data that is still stored as raw strings
are identifiers which are later resolved by normalizer as well since they require more context.
 */

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    End,
    Identifier(String),
    ConstInteger32(i32),
    ConstInteger64(i64),
    ConstFloat(f64),
    ConstString(Vec<PosChar>),
    ConstBoolean(bool),
    ConstChar(char),

    ParenthesisBlock(TokenBlock), // ()
    BracketBlock(TokenBlock),     // []
    BraceBlock(TokenBlock),       // {}

    // keywords
    If,
    Else,
    While,
    For,
    Out,
    Return,
    Struct,
    Fn,
    I32,
    I64,
    F32,
    F64,
    Void,
    String,
    Char,
    Bool,
    Operator,
    Macro,

    // symbols
    Plus,               // +
    Star,               // *
    Slash,              // /
    Assign,             // =
    Equals,             // ==
    NotEquals,          // !=
    LessThan,           // <
    LessThanOrEqual,    // <=
    GreaterThan,        // >
    GreaterThanOrEqual, // >=
    Minus,              // -
    Reference,          // &
    Dot,                // .
    Comma,              // ,
    PlusEquals,         // +=
    MinusEquals,        // -=
    MulEquals,          // *=
    DivEquals,          // /=
    ModEquals,          // %=
    Increment,          // ++
    Decrement,          // --
    Colon,              // :
    Semicolon,          // :
    QuestionMark,       // ?
    Pipe,               // |
    DotDot,             // ..
    And,                // &&
    Or,                 // ||
    Mod,                // %
    Not,                // !
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
        "fn" => Some(Token::Fn),
        "i32" => Some(Token::I32),
        "i64" => Some(Token::I64),
        "f32" => Some(Token::F32),
        "f64" => Some(Token::F64),
        "void" => Some(Token::Void),
        "_string" => Some(Token::String),
        "char" => Some(Token::Char),
        "bool" => Some(Token::Bool),
        "operator" => Some(Token::Operator),
        "macro" => Some(Token::Macro),
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
        ';' => Some(Token::Semicolon),
        '?' => Some(Token::QuestionMark),
        '|' => Some(Token::Pipe),
        ',' => Some(Token::Comma),
        '%' => Some(Token::Mod),
        '!' => Some(Token::Not),
        _ => None,
    }
}

const fn symbol_from_two_chars(c1: char, c2: char) -> Option<Token> {
    match (c1, c2) {
        ('=', '=') => Some(Token::Equals),
        ('!', '=') => Some(Token::NotEquals),
        ('<', '=') => Some(Token::LessThanOrEqual),
        ('>', '=') => Some(Token::GreaterThanOrEqual),
        ('+', '=') => Some(Token::PlusEquals),
        ('-', '=') => Some(Token::MinusEquals),
        ('*', '=') => Some(Token::MulEquals),
        ('/', '=') => Some(Token::DivEquals),
        ('%', '=') => Some(Token::ModEquals),
        ('+', '+') => Some(Token::Increment),
        ('-', '-') => Some(Token::Decrement),
        ('.', '.') => Some(Token::DotDot),
        ('&', '&') => Some(Token::And),
        ('|', '|') => Some(Token::Or),
        _ => None,
    }
}

// a block usually represents contents  an if statement, while loop, or function
// the whole program is also a block
// blocks are formed by indentation levels
#[derive(Debug, PartialEq, Clone)]
pub struct TokenBlock {
    tokens: Vec<(Token, FilePosition)>,
    last_pos: FilePosition,
}

const LAST_TOKEN: (Token, FilePosition) = (Token::End, FilePosition::unknown());

impl TokenBlock {
    pub fn new(mut tokens: Vec<(Token, FilePosition)>) -> Self {
        tokens.reverse();
        Self {
            tokens,
            last_pos: FilePosition::unknown(),
        }
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
        let res = self.tokens.pop().unwrap_or_else(|| LAST_TOKEN.clone());
        if res.0 != Token::End {
            self.last_pos = res.1;
        }
        res
    }

    pub const fn get_last_pos(&self) -> FilePosition {
        self.last_pos
    }

    pub fn has_tokens(&self) -> bool {
        self.peek().0 != Token::End
    }

    pub fn into_iter(mut self) -> Vec<(Token, FilePosition)> {
        self.tokens.reverse();
        self.tokens
    }
}

fn string_to_token(string: &String) -> Token {
    if let Some(keyword) = str_to_keyword(string) {
        return keyword;
    }

    if let Ok(integer) = string.parse::<i32>() {
        return Token::ConstInteger32(integer);
    }

    if let Ok(integer) = string.parse::<i64>() {
        return Token::ConstInteger64(integer);
    }

    // parse i64 notation: 1000L
    if string.ends_with('L') {
        let mut string = string.clone();
        string.pop();
        if let Ok(integer) = string.parse::<i64>() {
            return Token::ConstInteger64(integer);
        }
    }

    if let Ok(float) = string.parse::<f64>()
        && string.contains('.')
    {
        return Token::ConstFloat(float);
    }

    if string == "true" || string == "false" {
        return Token::ConstBoolean(string == "true");
    }

    Token::Identifier(string.clone())
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
            *token_pos += pos;
        }
        curr_token.push(c);
    };

    let mut iter = string.iter().peekable();
    while let Some(frag) = iter.next() {
        match frag {
            Fragment::String(s, pos) => {
                new_token(&mut tokens, &mut curr_token, &mut token_pos);
                tokens.push((Token::ConstString(s.clone()), *pos));
            }
            Fragment::Char(pos_char) => {
                let c = pos_char.c;
                let pos = &pos_char.pos;
                let (next_char, next_char_pos) = match iter.peek() {
                    Some(Fragment::Char(pc)) => (pc.c, pc.pos),
                    _ => ('\0', FilePosition::unknown()),
                };

                // in case we have range syntax: 0..10 we do not mistake it for a float
                let is_next_dot = matches!(iter.peek(), Some(Fragment::Char(c)) if c.c == '.');

                if c == '.' && curr_token.parse::<i32>().is_ok() && !is_next_dot {
                    // decimal point in a float
                    add_to_token(&mut curr_token, &mut token_pos, c, *pos);
                } else if let Some(symbol) = symbol_from_two_chars(c, next_char) {
                    new_token(&mut tokens, &mut curr_token, &mut token_pos);
                    tokens.push((symbol, *pos + next_char_pos));
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
            Fragment::ConstChar(c) => {
                new_token(&mut tokens, &mut curr_token, &mut token_pos);
                tokens.push((Token::ConstChar(c.c), c.pos));
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
