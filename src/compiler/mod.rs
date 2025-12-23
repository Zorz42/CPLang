#![warn(clippy::all)]
#![warn(clippy::pedantic)]
#![warn(clippy::nursery)]
#![warn(clippy::cargo)]
// most of the time i like it if there are no references,
// since the code looks prettier, especially in functions that consume arguments
#![allow(clippy::needless_pass_by_value)]
#![allow(clippy::similar_names)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::assigning_clones)]
#![allow(clippy::format_push_string)]

use crate::compiler::error::CompilerResult;
use crate::compiler::generator::generate_code;
use crate::compiler::lowerer::lower_ast;
use crate::compiler::normalizer::normalize_ast;
use crate::compiler::parser::parse_tokens;
use crate::compiler::preprocessor::preprocess;
use crate::compiler::tokenizer::{tokenize_fragments, TokenBlock};

pub mod error;
mod generator;
mod lowerer;
mod normalizer;
mod parser;
mod preprocessor;
mod tokenizer;
mod type_resolver;
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
    let input_sources = [
        include_str!("../core/operators.cpl"),
        &std::fs::read_to_string(input_file).unwrap(),
    ];

    let mut fragment_blocks = Vec::new();
    for input in input_sources {
        fragment_blocks.push(preprocess(&input)?)
    }

    let mut program_block = Vec::new();
    for block in fragment_blocks {
        let block = tokenize_fragments(&block.fragments)?;
        program_block.append(&mut block.into_iter());
    }

    let ast = parse_tokens(TokenBlock::new(program_block))?;
    let ast = lower_ast(ast);
    let ir = normalize_ast(ast)?;
    let code = generate_code(ir);

    std::fs::write(output_file, code).unwrap();

    Ok(())
}

pub fn compile(input_file: &str, output_file: &str) -> CompilerResult<()> {
    // Run compilation in a thread with a large stack size to handle deep recursion
    const STACK_SIZE: usize = 256 * 1024 * 1024; // 256MB

    let input_file = input_file.to_string();
    let output_file = output_file.to_string();

    std::thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(move || {
            let res = compile_internal(&input_file, &output_file);
            res
        })
        .unwrap()
        .join()
        .unwrap()
}
