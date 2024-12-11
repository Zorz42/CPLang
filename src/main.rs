use crate::compiler::compile;

mod compiler;
mod tests;

fn main() {
    let input_file = "main.cpl";
    let output_file = "main.c";
    compile(input_file, output_file);
}
