use crate::compiler::error::CompilerResult;
use crate::compiler::generator::generate_code;
use crate::compiler::parser::parse_tokens;
use crate::compiler::preprocessor::{add_file_positions, parse_indentation, parse_strings_and_comments};
use crate::compiler::tokenizer::tokenize_lines;

mod tokenizer;
mod parser;
mod generator;
mod preprocessor;
pub mod error;

pub fn compile(input_file: &str, output_file: &str) -> CompilerResult<()> {
    // read input file into a string
    let input = std::fs::read_to_string(input_file).unwrap();

    let input_with_positions = add_file_positions(&input);
    let fragments = parse_strings_and_comments(&input_with_positions)?;
    for f in &fragments {
        println!("{:?}", f);
    }
    let lines = parse_indentation(&fragments)?;
    let program_block = tokenize_lines(&lines)?;
    //println!("{:?}", program_block);
    let (functions, structs) = parse_tokens(&program_block)?;

    let code = generate_code(functions, structs)?;
    // write code to output file
    std::fs::write(output_file, code).unwrap();

    Ok(())
}