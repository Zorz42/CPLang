use crate::compiler::error::{CompilerError, FilePosition};
use crate::compiler::gain_input_sources;

const RED_BG: &str = "\x1b[41m";
const GREY_TEXT: &str = "\x1b[90m";
const RESET: &str = "\x1b[0m";
const RED_TEXT: &str = "\x1b[31m";
const BOLD_TEXT: &str = "\x1b[1m";

pub const INPUT_NAMES: [&str; 6] = [
    "",
    "src/core/operators.cpl",
    "src/core/range.cpl",
    "src/core/io.cpl",
    "src/core/vector.cpl",
    "src/core/string.cpl",
];

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
