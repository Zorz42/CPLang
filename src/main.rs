use std::process::ExitCode;

use crate::compiler::compile;
use crate::compiler::error::CompilerError;
use crate::display_error::display_error;

mod compiler;
mod display_error;
mod tests;

// Normalization clones a lot of small values (see the type resolver's rollback
// state), which makes the allocator a measurable part of compile time.
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

struct Args<'a> {
    input: &'a str,
    output: &'a str,
}

fn print_usage(program_name: &str) {
    eprintln!("Usage: {} <input_file> -o <output_file>", program_name);
}

/// Parses everything after the program name. The error is a message to show
/// alongside the usage line.
fn parse_args(args: &[String]) -> Result<Args<'_>, String> {
    let mut input_file: Option<&str> = None;
    let mut output_file: Option<&str> = None;

    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "-o" => {
                let value = args.get(i + 1).ok_or("Missing value for -o option")?;
                output_file = Some(value);
                i += 2;
            }
            arg => {
                if input_file.is_some() {
                    return Err(format!("Unexpected argument '{}'", arg));
                }
                input_file = Some(arg);
                i += 1;
            }
        }
    }

    match (input_file, output_file) {
        (Some(input), Some(output)) => Ok(Args { input, output }),
        (None, _) => Err("Missing input file".to_string()),
        (_, None) => Err("Missing output file option (-o)".to_string()),
    }
}

fn run(args: &[String]) -> ExitCode {
    let program_name = args.first().map(|s| s.as_str()).unwrap_or("cplang");

    let args = match parse_args(args.get(1..).unwrap_or_default()) {
        Ok(args) => args,
        Err(message) => {
            eprintln!("Error: {}", message);
            print_usage(program_name);
            return ExitCode::FAILURE;
        }
    };

    if let Err(err) = compile(args.input, args.output) {
        report_error(&err, args.input);
        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}

/// Renders a compiler error. Only a positioned error needs the source text, to
/// draw the snippet around it; an I/O error has nothing to point at, and its
/// input file may well be the thing that could not be read.
fn report_error(err: &CompilerError, input_path: &str) {
    if err.position.is_none() {
        display_error(err, input_path, "");
        return;
    }

    match std::fs::read_to_string(input_path) {
        Ok(content) => display_error(err, input_path, &content),
        Err(e) => eprintln!("Error reading input file '{}': {}", input_path, e),
    }
}

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().collect();
    let code = run(&args);

    #[cfg(feature = "count_calls")]
    cplang::call_counter::print_counts();

    code
}
