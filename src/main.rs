use crate::compiler::compile;
use crate::display_error::display_error;

mod compiler;
mod display_error;
mod tests;

// Normalization clones a lot of small values (see the type resolver's rollback
// state), which makes the allocator a measurable part of compile time.
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

fn print_usage(program_name: &str) {
    eprintln!("Usage: {} <input_file> -o <output_file>", program_name);
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let program_name = args.first().map(|s| s.as_str()).unwrap_or("cplang");

    let mut input_file: Option<&str> = None;
    let mut output_file: Option<&str> = None;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "-o" => {
                if i + 1 < args.len() {
                    output_file = Some(&args[i + 1]);
                    i += 2;
                } else {
                    eprintln!("Error: Missing value for -o option");
                    print_usage(program_name);
                    std::process::exit(1);
                }
            }
            arg => {
                if input_file.is_none() {
                    input_file = Some(arg);
                    i += 1;
                } else {
                    eprintln!("Error: Unexpected argument '{}'", arg);
                    print_usage(program_name);
                    std::process::exit(1);
                }
            }
        }
    }

    let (input, output) = match (input_file, output_file) {
        (Some(i), Some(o)) => (i, o),
        (None, _) => {
            eprintln!("Error: Missing input file");
            print_usage(program_name);
            std::process::exit(1);
        }
        (_, None) => {
            eprintln!("Error: Missing output file option (-o)");
            print_usage(program_name);
            std::process::exit(1);
        }
    };

    let res = compile(input, output);
    if let Err(err) = res {
        match std::fs::read_to_string(input) {
            Ok(content) => display_error(&err, input, &content),
            Err(e) => {
                eprintln!("Error reading input file '{}': {}", input, e);
                std::process::exit(1);
            }
        }
    }

    #[cfg(feature = "count_calls")]
    cplang::call_counter::print_counts();
}
