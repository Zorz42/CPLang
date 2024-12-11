use crate::compiler::compile;
use crate::compiler::error::display_error;

mod compiler;
mod tests;

fn main() {
    let input_file = "main.cpl";
    let output_file = "main.c";
    let res = compile(input_file, output_file);
    if let Err(err) = res {
        display_error(&err, &std::fs::read_to_string(input_file).unwrap());
    }
}
