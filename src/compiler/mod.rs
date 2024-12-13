use crate::compiler::error::CompilerResult;
use crate::compiler::generator::generate_code;
use crate::compiler::parser::parse_tokens;
use crate::compiler::preprocessor::{add_file_positions, parse_indentation, remove_comments};
use crate::compiler::tokenizer::tokenize_blocks;

mod tokenizer;
mod parser;
mod generator;
mod preprocessor;
pub mod error;

pub fn compile(input_file: &str, output_file: &str) -> CompilerResult<()> {
    // read input file into a string
    let input = std::fs::read_to_string(input_file).unwrap();

    let input_with_positions = add_file_positions(&input);
    let without_comments = remove_comments(input_with_positions)?;
    let lines = parse_indentation(without_comments)?;
    //println!("{:?}", lines);
    let program_block = tokenize_blocks(lines)?;
    //println!("{:?}", program_block);
    let functions = parse_tokens(&program_block)?;

    let code = generate_code(functions)?;
    // write code to output file
    std::fs::write(output_file, code).unwrap();

    Ok(())
}