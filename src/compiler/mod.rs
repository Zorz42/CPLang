use crate::compiler::error::CompilerResult;
use crate::compiler::generator::generate_code;
use crate::compiler::parser::parse_tokens;
use crate::compiler::preprocessor::preprocess;
use crate::compiler::tokenizer::tokenize_fragments;

mod tokenizer;
mod parser;
mod generator;
mod preprocessor;
pub mod error;
mod ir;
/*
The compiler works in the following steps:
1. Preprocessing: The input source code is preprocessed to parse strings, comments, indentation and parses bracket/brace/parenthesis structure.
2. Tokenization: The preprocessed fragments are tokenized into a stream of tokens.
3. Parsing: The token stream is parsed into an Abstract Syntax Tree (AST) representing the program structure.
4. Normalization: Syntactic sugar is transformed into more rudimentary operations. For example for loop -> while loop
Names/labels are resolved. AST is transformed into IR (Immediate representation), which is AST with less different types of nodes.
5. Type resolution: All types are resolved so every type is now known.
6. Code generation: IR with types is converted to C code.
 */

pub fn compile(input_file: &str, output_file: &str) -> CompilerResult<()> {
    // read input file into a string
    let input = std::fs::read_to_string(input_file).unwrap();

    let fragment_block = preprocess(&input)?;
    let program_block = tokenize_fragments(&fragment_block.fragments)?;
    //println!("{:?}", program_block);
    let (functions, structs) = parse_tokens(&program_block)?;

    let code = generate_code(functions, structs)?;
    // write code to output file
    std::fs::write(output_file, code).unwrap();

    Ok(())
}