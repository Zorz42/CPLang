use crate::compiler::{gain_input_sources, INPUT_NAMES};
use std::ops::{Add, AddAssign};

// this struct stores file position so the error can be displayed
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FilePosition {
    pub file_ident: usize,
    pub first_pos: (usize, usize),
    pub last_pos: (usize, usize),
}

impl FilePosition {
    pub const fn unknown() -> Self {
        Self {
            file_ident: 0,
            first_pos: (usize::MAX, usize::MAX),
            last_pos: (usize::MAX, usize::MAX),
        }
    }
}

impl Add for FilePosition {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        if self == Self::unknown() {
            return rhs;
        }
        if rhs == Self::unknown() {
            return self;
        }
        assert_eq!(self.file_ident, rhs.file_ident);
        Self {
            file_ident: self.file_ident,
            first_pos: <(usize, usize)>::min(self.first_pos, rhs.first_pos),
            last_pos: <(usize, usize)>::max(self.last_pos, rhs.last_pos),
        }
    }
}

impl AddAssign for FilePosition {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

#[derive(Debug)]
pub struct CompilerError {
    pub message: String,
    pub position: Option<FilePosition>,
}

// this is a universal result type for the compiler
pub type CompilerResult<T> = Result<T, CompilerError>;

const RED_BG: &str = "\x1b[41m";
const GREY_TEXT: &str = "\x1b[90m";
const RESET: &str = "\x1b[0m";
const RED_TEXT: &str = "\x1b[31m";
const BOLD_TEXT: &str = "\x1b[1m";

pub fn display_error(error: &CompilerError, input_path: &str, input: &str) {
    println!("{BOLD_TEXT}{RED_TEXT}Error: {}{RESET}", error.message);

    if let Some(position) = &error.position {
        let line_start = position.first_pos.0;
        let line_end = position.last_pos.0;

        if *position == FilePosition::unknown() {
            unreachable!();
        }

        let mut file_name = INPUT_NAMES[position.file_ident];
        if file_name.is_empty() {
            file_name = input_path;
        }
        println!("  --> {file_name}:{}:{}", position.first_pos.0, position.first_pos.1 + 1);
        println!();

        let source = &gain_input_sources(input.to_string())[position.file_ident];
        let lines: Vec<&str> = source.lines().collect();

        let padded_line_start = line_start.saturating_sub(2);
        let padded_line_end = (line_end + 2).min(lines.len() - 1);

        // this lint only makes things less readable
        #[allow(clippy::needless_range_loop)]
        for line in padded_line_start..=padded_line_end {
            // first, print line number and leave space after that for longer line numbers
            let spacing = " ".repeat((padded_line_end + 1).to_string().len() - (line + 1).to_string().len());
            print!("  {GREY_TEXT}{}{spacing} | {RESET}", line + 1);

            for (idx, ch) in lines[line].chars().enumerate() {
                if position.first_pos <= (line, idx) && (line, idx) < position.last_pos {
                    print!("{RED_BG}{ch}{RESET}");
                } else {
                    print!("{ch}");
                }
            }
            println!();
        }
    }
}
