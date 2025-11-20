// this struct stores file position so the error can be displayed
#[derive(Debug, Clone, PartialEq)]
pub struct FilePosition {
    pub first_pos: (usize, usize),
    pub last_pos: (usize, usize),
}

impl FilePosition {
    pub fn unknown() -> Self {
        Self {
            first_pos: (usize::MAX, usize::MAX),
            last_pos: (usize::MAX, usize::MAX),
        }
    }
}

pub fn merge_file_positions(position1: &FilePosition, position2: &FilePosition) -> FilePosition {
    if *position1 == FilePosition::unknown() {
        return position2.clone();
    }
    if *position2 == FilePosition::unknown() {
        return position1.clone();
    }
    FilePosition {
        first_pos: position1.first_pos.min(position2.first_pos),
        last_pos: position1.last_pos.max(position2.last_pos),
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

pub fn display_error(error: &CompilerError, input: &str) {
    println!("{BOLD_TEXT}{RED_TEXT}Error: {}{RESET}", error.message);

    if let Some(position) = &error.position {
        let line_start = position.first_pos.0;
        let line_end = position.last_pos.0;

        if *position == FilePosition::unknown() {
            unreachable!();
        }

        let lines: Vec<&str> = input.lines().collect();

        let padded_line_start = line_start.saturating_sub(2);
        let padded_line_end = (line_end + 2).min(lines.len() - 1);

        // this lint only makes things less readable
        #[allow(clippy::needless_range_loop)]
        for line in padded_line_start..=padded_line_end {
            // first, print line number and leave space after that for longer line numbers
            let spacing = " ".repeat((padded_line_end + 1).to_string().len() - (line + 1).to_string().len());
            print!("{GREY_TEXT}{}{spacing}| {RESET}", line + 1);

            for (idx, ch) in lines[line].chars().enumerate() {
                if position.first_pos <= (line, idx) && (line, idx) < position.last_pos {
                    print!("{RED_BG}{}{RESET}", ch);
                } else {
                    print!("{}", ch);
                }
            }
            println!();
        }
    }
}
