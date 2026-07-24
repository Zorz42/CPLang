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
#![allow(clippy::cast_sign_loss)]

use crate::compiler::codegen::generate_code;
use crate::compiler::error::CompilerResult;
use crate::compiler::lowerer::lower_ast;
use crate::compiler::macros::insert_macros;
use crate::compiler::normalizer::normalize_ast;
use crate::compiler::parser::parse_tokens;
use crate::compiler::preprocessor::preprocess;
use crate::compiler::tokenizer::{TokenBlock, tokenize_fragments};

mod codegen;
pub mod error;
mod lowerer;
mod macros;
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

pub fn gain_input_sources(input_content: String) -> [String; 7] {
    [
        input_content,
        include_str!("../core/operators.cpl").to_string(),
        include_str!("../core/range.cpl").to_string(),
        include_str!("../core/io.cpl").to_string(),
        include_str!("../core/vector.cpl").to_string(),
        include_str!("../core/string.cpl").to_string(),
        include_str!("../core/print.cpl").to_string(),
    ]
}

fn compile_internal(input_file: &str, output_file: &str) -> CompilerResult<()> {
    let input_content = std::fs::read_to_string(input_file).unwrap();
    let input_sources = gain_input_sources(input_content);

    let mut fragment_blocks = Vec::new();
    for (file_ident, input) in input_sources.into_iter().enumerate() {
        fragment_blocks.push(preprocess(&input, file_ident)?);
    }

    let mut program_block = Vec::new();
    for block in fragment_blocks {
        let block = tokenize_fragments(&block.fragments)?;
        program_block.append(&mut block.into_iter());
    }

    let program_block = insert_macros(program_block)?;

    let ast = parse_tokens(TokenBlock::new(program_block))?;
    let ast = lower_ast(ast);
    let ir = normalize_ast(ast)?;
    let code = generate_code(ir);

    std::fs::write(output_file, code).unwrap();

    Ok(())
}

/// Compiles `input_file` into `output_file`. Both are paths.
/// # Panics
/// Should never panic
///
/// # Errors
/// Returns compilation error
pub fn compile(input_file: &str, output_file: &str) -> CompilerResult<()> {
    // Run compilation in a thread with a large stack size to handle deep recursion
    const STACK_SIZE: usize = 256 * 1024 * 1024; // 256MB

    let input_file = input_file.to_string();
    let output_file = output_file.to_string();

    std::thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(move || {
            // only change this if you are profiling and need more samples
            const NUM_ITERS: usize = 1;
            for _ in 0..NUM_ITERS {
                compile_internal(&input_file, &output_file)?;
            }
            Ok(())
        })
        .unwrap()
        .join()
        .unwrap()
}
