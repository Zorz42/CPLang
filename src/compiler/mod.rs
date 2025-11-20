use crate::compiler::error::CompilerResult;
use crate::compiler::generator::generate_code;
use crate::compiler::lowerer::lower_ast;
use crate::compiler::normalizer::normalize_ast;
use crate::compiler::parser::parse_tokens;
use crate::compiler::preprocessor::preprocess;
use crate::compiler::tokenizer::tokenize_fragments;

pub mod error;
mod generator;
mod lowerer;
mod normalizer;
mod parser;
mod preprocessor;
mod tokenizer;
/*
The compiler works in the following steps:
1. Preprocessing: The input source code is preprocessed to parse strings, comments, indentation and parses bracket/brace/parenthesis structure.
2. Tokenization: The preprocessed fragments are tokenized into a tree of tokens.
3. Parsing: The token stream is parsed into an Abstract Syntax Tree (AST) representing the program structure.
4. Lowering: Syntactic sugar is transformed into more rudimentary operations. For example for loop -> while loop, a += 1 -> a = a + 1
5. Normalization: Names/labels and types are resolved/deduced. AST is transformed into IR (Immediate representation), which is AST with
less different types of nodes and explicit types and indexes instead of string/name labels.
6. Code generation: IR is converted to C code.
 */

fn compile_internal(input_file: &str, output_file: &str) -> CompilerResult<()> {
    let input = std::fs::read_to_string(input_file).unwrap();

    let fragment_block = preprocess(&input)?;

    println!("{:?}", fragment_block);

    let program_block = tokenize_fragments(&fragment_block.fragments)?;

    println!("{:?}", program_block);

    let ast = parse_tokens(&program_block)?;
    let ast = lower_ast(ast);

    //println!("{:?}", ast);

    let ir = normalize_ast(ast)?;

    //println!("{:?}", ir);

    let code = generate_code(ir);

    //println!("{code}");

    std::fs::write(output_file, code).unwrap();

    Ok(())
}

pub fn compile(input_file: &str, output_file: &str) -> CompilerResult<()> {
    // Run compilation in a thread with a large stack size to handle deep recursion
    // 256MB stack size (default is usually 2-8MB) - effectively unlimited for practical purposes
    const STACK_SIZE: usize = 256 * 1024 * 1024; // 256MB

    let input_file = input_file.to_string();
    let output_file = output_file.to_string();

    std::thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(move || compile_internal(&input_file, &output_file))
        .unwrap()
        .join()
        .unwrap()
}
