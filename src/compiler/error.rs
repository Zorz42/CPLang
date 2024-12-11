use std::collections::HashSet;

// this struct stores file position so the error can be displayed
#[derive(Debug, Clone)]
pub struct FilePosition {
    pub positions: Vec<(usize, usize)>,
}

pub fn merge_file_positions(position1: &FilePosition, position2: &FilePosition) -> FilePosition {
    let mut merged = position1.clone();
    merged.positions.extend(position2.clone().positions);
    merged
}

pub struct CompilerError {
    pub message: String,
    pub position: FilePosition,
}

// this is a universal result type for the compiler
pub type CompilerResult<T> = Result<T, CompilerError>;

const RED_BG: &str = "\x1b[41m";
const GREY_TEXT: &str = "\x1b[90m";
const RESET: &str = "\x1b[0m";
const RED_TEXT: &str = "\x1b[31m";
const BOLD_TEXT: &str = "\x1b[1m";

pub fn display_error(error: &CompilerError, input: &str) {
    let mut positions = HashSet::new();

    let mut line_start = usize::MAX;
    let mut line_end = 0;

    for (line, column) in &error.position.positions {
        positions.insert((*line, *column));
        line_start = line_start.min(*line);
        line_end = line_end.max(*line);
    }

    let lines: Vec<&str> = input.lines().collect();

    println!("{BOLD_TEXT}{RED_TEXT}Error: {}{RESET}", error.message);

    for line in line_start.saturating_sub(2)..=(line_end + 2).min(lines.len() - 1) {
        // first, print line number and leave space after that for longer line numbers
        let spacing = " ".repeat((line_end + 1).to_string().len() - (line + 1).to_string().len());
        print!("{GREY_TEXT}{}{spacing}|{RESET}", line + 1);

        for (idx, ch) in lines[line].chars().enumerate() {
            if positions.contains(&(line, idx)) {
                print!("{RED_BG}{}{RESET}", ch);
            } else {
                print!("{}", ch);
            }
        }
        println!();
    }
}