use std::fmt::{Debug, Formatter};
use std::ops::{Add, AddAssign};

// this struct stores file position so the error can be displayed
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct FilePosition {
    pub file_ident: usize,
    pub first_pos: (usize, usize),
    pub last_pos: (usize, usize),
}

impl Debug for FilePosition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("()")
    }
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
