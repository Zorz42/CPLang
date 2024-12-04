use crate::compiler::parser::parse_tokens;
use crate::compiler::tokenizer::parse_blocks;

mod tokenizer;
mod parser;

// splits input into lines and parses indentation levels
fn parse_indentation(input: &str) -> Vec<(i32, String)> {
    let mut lines = Vec::new();
    for line in input.lines() {
        let mut indent = 0;
        let leading_spaces = line.chars().take_while(|c| *c == ' ').count();
        if leading_spaces % 4 != 0 {
            panic!("Invalid indentation");
        }

        indent = (leading_spaces / 4) as i32;
        let line = line.trim().to_string();
        if !line.is_empty() {
            lines.push((indent, line));
        }
    }
    lines
}

pub fn compile(input_file: &str, output_file: &str) {
    // read input file into a string
    let input = std::fs::read_to_string(input_file).unwrap();

    let lines = parse_indentation(&input);
    println!("{:?}", lines);
    let program_block = parse_blocks(lines);
    println!("{:?}", program_block);
    parse_tokens(&program_block);
}