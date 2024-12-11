// this struct stores file position so the error can be displayed
pub struct FilePosition {
    pub line_start: usize,
    pub line_end: usize,
    pub column_start: usize,
    pub column_end: usize,
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

pub fn display_error(error: &CompilerError, input: &str) {
    let lines: Vec<&str> = input.lines().collect();
    let line_start = error.position.line_start;
    let line_end = error.position.line_end;
    let column_start = error.position.column_start;
    let column_end = error.position.column_end;

    println!("Error: {}", error.message);

    for line in line_start.saturating_sub(2)..=(error.position.line_end + 2).min(lines.len() - 1) {
        // first, print line number
        print!("{}{} |{}", GREY_TEXT, line + 1, RESET);

        if line < line_start || line > line_end {
            // no red background
            println!("{}", lines[line]);
            continue;
        }

        let begin = if line == line_start { column_start } else { 0 };
        let end = if line == line_end { column_end } else { lines[line].len() };

        // have red background from begin to end
        println!("{}{}{}{}{}", &lines[line][..begin], RED_BG, &lines[line][begin..end], RESET, &lines[line][end..]);

    }
}