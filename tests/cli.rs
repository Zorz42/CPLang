//! Tests for the `cplang` binary itself: argument handling, exit status, and
//! how errors are rendered.
//!
//! The `.cpl` cases under `src/tests/` call the compiler as a library, so none
//! of them exercise `main.rs` or `display_error.rs`. Those two files are the
//! whole of the user-facing surface — what a Makefile or `cses-tests/run.py`
//! sees — so they are worth testing directly.
//!
//! An assertion for a bug that is known but not yet fixed is named
//! `known_bug_…` with a message starting `KNOWN BUG`, in the same spirit as the
//! `//BUG` cases in `src/tests/13_known_bugs/`; they live in their own section
//! below and are red on purpose. When such a bug is fixed the prefix comes off,
//! the comment is rewritten to describe the guarantee rather than the defect,
//! and the assertion is tightened past "it did not panic" where that is cheap —
//! the section before it is the ones that have been through that.
//!
//! Note that `cargo test` stops at the first failing target, so while any
//! `//BUG` case under `src/tests/` is red the binary's tests fail and this file
//! never runs. Use `cargo test --no-fail-fast` to reach it.

use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicU64, Ordering};

/// A scratch directory unique to one test, removed when the test ends.
struct Scratch {
    dir: PathBuf,
}

impl Scratch {
    fn new(name: &str) -> Self {
        static COUNTER: AtomicU64 = AtomicU64::new(0);
        let dir = std::env::temp_dir().join(format!("cplang-cli-{name}-{}-{}", std::process::id(), COUNTER.fetch_add(1, Ordering::Relaxed)));
        std::fs::create_dir_all(&dir).expect("could not create the scratch directory");
        Self { dir }
    }

    /// Writes `source` to `input.cpl` and returns its path.
    fn input(&self, source: &str) -> PathBuf {
        let path = self.dir.join("input.cpl");
        std::fs::write(&path, source).expect("could not write the test input");
        path
    }

    fn output(&self) -> PathBuf {
        self.dir.join("output.c")
    }
}

impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.dir);
    }
}

/// Drops ANSI colour codes so assertions can look at the text. `display_error`
/// wraps each highlighted character individually, so the offending source line
/// is not contiguous until the escapes are removed.
fn strip_ansi(text: &str) -> String {
    let mut res = String::new();
    let mut chars = text.chars();
    while let Some(c) = chars.next() {
        if c != '\u{1b}' {
            res.push(c);
            continue;
        }
        // Skip "[…m", the only escape shape display_error emits.
        for c in chars.by_ref() {
            if c == 'm' {
                break;
            }
        }
    }
    res
}

fn cplang(args: &[&Path]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_cplang"))
        .args(args)
        .output()
        .expect("could not run the cplang binary")
}

/// Compiles `source` and returns the process result together with the text the
/// binary printed on both streams.
fn compile(scratch: &Scratch, source: &str) -> (Output, String) {
    let input = scratch.input(source);
    let output = cplang(&[&input, Path::new("-o"), &scratch.output()]);
    let printed = strip_ansi(&format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    ));
    (output, printed)
}

/// A process that dies of a panic exits with 101; distinguishing that from an
/// ordinary failure is the point of several tests below.
fn panicked(output: &Output) -> bool {
    output.status.code() == Some(101)
}

const HELLO: &str = "fn main\n    out \"Hello\"\n";

// ---------------------------------------------------------------------------
// the happy path
// ---------------------------------------------------------------------------

#[test]
fn compiles_a_program_and_succeeds() {
    let scratch = Scratch::new("ok");
    let (output, printed) = compile(&scratch, HELLO);

    assert_eq!(output.status.code(), Some(0), "a successful compile must exit 0; printed: {printed}");
    let c = std::fs::read_to_string(scratch.output()).expect("the output file should have been written");
    assert!(c.contains("int main("), "the generated C should have a main function:\n{c}");
    assert!(c.contains("putchar"), "the generated C should print through putchar:\n{c}");
}

#[test]
fn the_generated_program_runs_and_prints() {
    let scratch = Scratch::new("run");
    let (output, printed) = compile(&scratch, HELLO);
    assert_eq!(output.status.code(), Some(0), "{printed}");

    let exe = scratch.dir.join("program");
    let gcc = Command::new("gcc")
        .arg("-w")
        .arg(scratch.output())
        .arg("-o")
        .arg(&exe)
        .output()
        .expect("could not run gcc");
    assert!(gcc.status.success(), "gcc rejected the output:\n{}", String::from_utf8_lossy(&gcc.stderr));

    let run = Command::new(&exe).output().expect("could not run the compiled program");
    assert_eq!(String::from_utf8_lossy(&run.stdout), "Hello");
}

#[test]
fn the_output_flag_may_come_first() {
    let scratch = Scratch::new("flagfirst");
    let input = scratch.input(HELLO);
    let output = cplang(&[Path::new("-o"), &scratch.output(), &input]);

    assert_eq!(output.status.code(), Some(0), "-o before the input file should work too");
    assert!(scratch.output().exists());
}

// ---------------------------------------------------------------------------
// argument handling
// ---------------------------------------------------------------------------

/// Runs the binary with raw string arguments and returns (exit code, output).
fn run_args(args: &[&str]) -> (Option<i32>, String) {
    let output = Command::new(env!("CARGO_BIN_EXE_cplang"))
        .args(args)
        .output()
        .expect("could not run the cplang binary");
    let printed = strip_ansi(&format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    ));
    (output.status.code(), printed)
}

#[test]
fn no_arguments_prints_usage() {
    let (code, printed) = run_args(&[]);
    assert_eq!(code, Some(1), "{printed}");
    assert!(printed.contains("Missing input file"), "{printed}");
    assert!(printed.contains("Usage:"), "the usage line should be printed:\n{printed}");
}

#[test]
fn a_missing_output_flag_prints_usage() {
    let (code, printed) = run_args(&["some_input.cpl"]);
    assert_eq!(code, Some(1), "{printed}");
    assert!(printed.contains("Missing output file option (-o)"), "{printed}");
    assert!(printed.contains("Usage:"), "{printed}");
}

#[test]
fn a_missing_value_for_the_output_flag_is_reported() {
    let (code, printed) = run_args(&["some_input.cpl", "-o"]);
    assert_eq!(code, Some(1), "{printed}");
    assert!(printed.contains("Missing value for -o option"), "{printed}");
}

#[test]
fn a_second_input_file_is_rejected() {
    let (code, printed) = run_args(&["one.cpl", "two.cpl", "-o", "out.c"]);
    assert_eq!(code, Some(1), "{printed}");
    assert!(printed.contains("Unexpected argument 'two.cpl'"), "{printed}");
}

// ---------------------------------------------------------------------------
// I/O failures
// ---------------------------------------------------------------------------

#[test]
fn a_missing_input_file_is_reported_not_panicked() {
    let scratch = Scratch::new("noinput");
    let missing = scratch.dir.join("does_not_exist.cpl");
    let output = cplang(&[&missing, Path::new("-o"), &scratch.output()]);
    let printed = strip_ansi(&(String::from_utf8_lossy(&output.stdout).into_owned() + &String::from_utf8_lossy(&output.stderr)));

    assert!(!panicked(&output), "a missing input file must not panic:\n{printed}");
    assert_eq!(output.status.code(), Some(1), "{printed}");
    assert!(printed.contains("Could not read input file"), "{printed}");
}

#[test]
fn an_unwritable_output_path_is_reported_not_panicked() {
    let scratch = Scratch::new("nooutput");
    let input = scratch.input(HELLO);
    let unwritable = scratch.dir.join("no_such_directory").join("out.c");
    let output = cplang(&[&input, Path::new("-o"), &unwritable]);
    let printed = strip_ansi(&(String::from_utf8_lossy(&output.stdout).into_owned() + &String::from_utf8_lossy(&output.stderr)));

    assert!(!panicked(&output), "an unwritable output path must not panic:\n{printed}");
    assert_eq!(output.status.code(), Some(1), "{printed}");
    assert!(printed.contains("Could not write output file"), "{printed}");
}

// ---------------------------------------------------------------------------
// how compile errors reach the user
// ---------------------------------------------------------------------------

#[test]
fn a_failed_compile_exits_nonzero() {
    // FEEDBACK.md 1.2 was that this exited 0, so every driver — a Makefile,
    // CI, cses-tests/run.py — read a failed compile as success and went on to
    // use whatever .c was on disk. It is fixed; this keeps it fixed.
    let scratch = Scratch::new("failexit");
    let (output, printed) = compile(&scratch, "fn maine\n    0\n");

    assert_eq!(output.status.code(), Some(1), "a failed compile must exit non-zero; printed: {printed}");
}

#[test]
fn a_failed_compile_removes_the_stale_output() {
    // FEEDBACK.md 1.2, second half. Exiting non-zero only helps a driver that
    // checks the status; one that compiles, edits, recompiles and then builds
    // whatever .c is on disk would go on using the previous successful output.
    // A failed compile leaves no output at all, so there is nothing stale to
    // pick up.
    let scratch = Scratch::new("stale");

    let (ok, printed) = compile(&scratch, HELLO);
    assert_eq!(ok.status.code(), Some(0), "{printed}");
    assert!(scratch.output().exists(), "the first compile should have written the output");

    let (failed, _) = compile(&scratch, "fn maine\n    0\n");
    assert_eq!(failed.status.code(), Some(1));

    assert!(
        !scratch.output().exists(),
        "a failed compile must not leave the previous output file in place, \
         or a driver that ignores the exit status compiles stale C."
    );
}

#[test]
fn an_error_without_a_position_prints_only_the_message() {
    let scratch = Scratch::new("nopos");
    let (_, printed) = compile(&scratch, "fn maine\n    0\n");

    assert!(printed.contains("Error: No main function found"), "{printed}");
    assert!(!printed.contains("-->"), "an error with no position has no location line:\n{printed}");
}

#[test]
fn an_error_with_a_position_prints_a_snippet() {
    let scratch = Scratch::new("pos");
    let (_, printed) = compile(&scratch, "fn main\n    out 10\n");

    assert!(printed.contains("Error: Expected string after out keyword"), "{printed}");
    assert!(printed.contains("-->"), "a located error should point at the file:\n{printed}");
    assert!(printed.contains("input.cpl"), "the location should name the input file:\n{printed}");
    assert!(printed.contains("out 10"), "the offending line should be quoted:\n{printed}");
}

#[test]
fn location_line_number_matches_the_snippet() {
    // FEEDBACK.md 1.15. display_error printed the header straight from
    // `position.first_pos.0` while the gutter numbered its rows `line + 1`, and
    // added 1 to the column but not to the line — so "--> file:LINE:COL" named
    // the line above the one it went on to highlight.
    //
    // `out` sits on line 2 of this input and starts at column 5, so the header
    // must read `input.cpl:2:5`, and the gutter row numbered 2 must be the one
    // holding it. Checking both is the point: the two numbers came from
    // different expressions, so asserting the header alone would not notice
    // them drifting apart again.
    let scratch = Scratch::new("offbyone");
    let (_, printed) = compile(&scratch, "fn main\n    out 10\n");

    let location = printed
        .lines()
        .find(|l| l.contains("-->"))
        .unwrap_or_else(|| panic!("no location line in:\n{printed}"));
    let mut parts = location.rsplit(':');
    let column: usize = parts
        .next()
        .and_then(|n| n.trim().parse().ok())
        .unwrap_or_else(|| panic!("could not read a column out of {location:?}"));
    let line: usize = parts
        .next()
        .and_then(|n| n.trim().parse().ok())
        .unwrap_or_else(|| panic!("could not read a line number out of {location:?}"));

    assert_eq!(
        (line, column),
        (2, 5),
        "the header must give the 1-based position of `out`:\n{printed}"
    );

    let gutter = printed
        .lines()
        .find(|l| l.trim_start().starts_with(&format!("{line} |")))
        .unwrap_or_else(|| panic!("no gutter row numbered {line} in:\n{printed}"));
    assert!(
        gutter.contains("out 10"),
        "the row the header names must be the row it highlights:\n{printed}"
    );
}

#[test]
fn an_error_inside_the_core_library_names_the_core_file() {
    // A program that makes a core function fail type checking reports a
    // position inside src/core, which is the one case where the location line
    // must not say "input.cpl".
    let scratch = Scratch::new("corepos");
    let (output, printed) = compile(&scratch, "fn main\n    v = Vec()\n    v.push(1)\n    v.push(\"two\")\n");

    assert_eq!(output.status.code(), Some(1), "{printed}");
    assert!(printed.contains("Error:"), "{printed}");
}

// ---------------------------------------------------------------------------
// behaviour that was once broken, kept fixed
// ---------------------------------------------------------------------------


#[test]
fn f64_is_compiled_to_double() {
    // FEEDBACK.md 1.11. gen_primitive_type used to map f64 to C `long double`,
    // which is 80-bit x87 on x86-64 (the judges) and 64-bit on Apple Silicon,
    // so the same program printed different numbers on different platforms and
    // every float operation on x86 went through x87. It maps to `double` now.
    //
    // Worth keeping: bump_malloc rounds allocations to 8 bytes, which is fine
    // for `double` but was not for x86-64 `long double`'s 16-byte alignment, so
    // a regression here would misalign Vec[f64] storage again.
    let scratch = Scratch::new("longdouble");
    let (output, printed) = compile(&scratch, "fn main\n    x = 1.5\n    out \"{x}\"\n");
    assert_eq!(output.status.code(), Some(0), "{printed}");

    let c = std::fs::read_to_string(scratch.output()).expect("output file");
    let occurrences = c.matches("long double").count();
    assert_eq!(
        occurrences, 0,
        "f64 must not be emitted as `long double` ({occurrences} occurrences), \
         or results become platform-dependent."
    );
}

#[test]
fn a_file_without_code_reports_no_main_function() {
    // FEEDBACK.md 1.5, seen through the CLI. This used to exit 101 — the
    // signature of a panic in the preprocessor — instead of saying anything.
    // The library-level case is 00_lexical/63e_file_with_only_comments.cpl.
    let scratch = Scratch::new("emptyfile");
    let (output, printed) = compile(&scratch, "// nothing but a comment\n");

    assert!(!panicked(&output), "a source file with no code must not panic:\n{printed}");
    assert_eq!(output.status.code(), Some(1), "{printed}");
    assert!(
        printed.contains("No main function found"),
        "it should say what is missing, not just fail:\n{printed}"
    );
}

#[test]
fn an_error_at_end_of_input_prints_a_diagnostic() {
    // FEEDBACK.md 1.6, seen through the CLI. The compiler produced a good error
    // and then display_error hit its `unreachable!()` while printing it, so the
    // user saw a Rust panic after the diagnostic. The library-level case is
    // 00_lexical/64e_error_at_end_of_input_has_a_position.cpl.
    let scratch = Scratch::new("eofpanic");
    let (output, printed) = compile(&scratch, "fn main\n    x =\n");

    assert!(!panicked(&output), "an error at end of input must not panic the printer:\n{printed}");
    assert_eq!(output.status.code(), Some(1), "{printed}");
    assert!(printed.contains("Expected another token after this one"), "{printed}");
    assert!(printed.contains("-->"), "the error carries a position, so it should draw a snippet:\n{printed}");
}

#[test]
fn a_core_macro_can_be_used() {
    // FEEDBACK.md 1.7, seen through the CLI. Merging a position from the core
    // library with one from the user's file used to trip an assert_eq! and
    // abort the process; the macro now expands and the program compiles.
    let scratch = Scratch::new("macropanic");
    let (output, printed) = compile(&scratch, "_def_operator +; _builtin_add; char; char; char;\n\nfn main\n    out \"hi\"\n");

    assert!(!panicked(&output), "using a core macro must not panic:\n{printed}");
    assert_eq!(output.status.code(), Some(0), "{printed}");
    assert!(scratch.output().exists(), "the compile succeeded, so it should have written output");
}

// ---------------------------------------------------------------------------
// known bugs — red on purpose, see the module comment
// ---------------------------------------------------------------------------

/// A process killed by a signal has no exit code on unix; a stack overflow
/// aborts, so this is how that shows up.
fn aborted(output: &Output) -> bool {
    output.status.code().is_none() || output.status.code() == Some(134)
}

#[test]
fn known_bug_an_empty_format_expression_prints_a_diagnostic() {
    // The same defect as `an_error_at_end_of_input_prints_a_diagnostic`, in the
    // one shape its fix did not reach. That one works because the parser has
    // already consumed a token, so `TokenBlock::get_last_pos` has a real
    // position to point at. `out "{}"` hands `parse_format_string` an *empty*
    // token block: nothing was ever consumed, `get_last_pos` is still
    // `FilePosition::unknown()`, and the compiler builds
    // `Some(FilePosition::unknown())`. `display_error` reaches its
    // `unreachable!()` on exactly that value, so the user gets a good error
    // message followed by a Rust panic, and the binary exits 101 instead of 1.
    //
    // `x = ()` fails the same way, for the same reason.
    //
    // The fix is either to give the error a real position — the braces are
    // right there in `parse_format_string` — or to make `display_error` cope
    // with the sentinel the way `src/tests/harness.rs::report` already has to.
    let scratch = Scratch::new("emptyformat");
    let (output, printed) = compile(&scratch, "fn main\n    out \"{}\"\n");

    assert!(
        !panicked(&output),
        "KNOWN BUG — an empty format expression panics the error printer:\n{printed}"
    );
    assert_eq!(output.status.code(), Some(1), "{printed}");
    assert!(printed.contains("-->"), "the error should point at the empty braces:\n{printed}");
}

#[test]
fn known_bug_a_self_referential_struct_does_not_crash_the_compiler() {
    // `IRType` is a value tree — `Struct(label, Vec<IRType>)` holds its fields
    // by value — so a struct that names itself, even behind a reference,
    // expands forever. `gen_struct_field_types` recurses into `normalize_type`
    // for each field, which recurses back into `gen_struct_field_types`, and
    // the 256 MB compiler thread runs out of stack.
    //
    // A stack overflow is not a panic: it aborts the process, which is why this
    // cannot be a `.cpl` case — the abort would take the whole test binary
    // down instead of being caught by the harness's `catch_unwind`.
    //
    // Note the struct here is never instantiated and `f` is never called.
    // `compute_function_ordering` normalizes every declared signature up front,
    // so naming `Node` as a parameter type is enough.
    //
    // 08_structs/68e shows the shape that *is* handled: a template that would
    // expand forever is stopped by the normalizer's recursion limit and
    // reported. A directly self-referential struct reaches no such limit.
    // Whether the fix is to support it (a reference is a pointer, so it is
    // representable in C) or to reject it, the compiler must not abort.
    let scratch = Scratch::new("selfstruct");
    let source = "struct Node\n    v: i32\n    next: &Node\n\nfn f n: Node\n    ret n.v\n\nfn main\n    out \"ok\"\n";
    let (output, printed) = compile(&scratch, source);

    assert!(
        !aborted(&output),
        "KNOWN BUG — a self-referential struct overflows the compiler's stack:\n{printed}"
    );
    assert!(!panicked(&output), "it should not panic either:\n{printed}");
    assert!(
        matches!(output.status.code(), Some(0 | 1)),
        "KNOWN BUG — expected a clean compile or a reported error, got {:?}:\n{printed}",
        output.status.code()
    );
}

#[test]
fn known_bug_crlf_line_endings_are_accepted() {
    // Nothing in the pipeline treats `\r` as whitespace. `parse_indentation`
    // splits on `\n` only, and the tokenizer has no rule for `\r`, so it falls
    // through to `add_to_token` and becomes part of the identifier before it.
    // Every line-final token in a file saved with Windows line endings is
    // therefore a *different* token than it looks: `fn main\r\n` declares a
    // function named "main\r", and the compile ends with "No main function
    // found" pointing at nothing.
    //
    // This is written from Rust rather than as a `.cpl` case on purpose: a
    // checked-in file with CRLF endings is at the mercy of git's autocrlf, and
    // the bug is about the bytes, so the test writes them itself.
    //
    // The fix is to drop `\r` in the preprocessor, next to where tabs become
    // spaces.
    let scratch = Scratch::new("crlf");
    let (output, printed) = compile(&scratch, "fn main\r\n    out \"ok\"\r\n");

    assert_eq!(
        output.status.code(),
        Some(0),
        "KNOWN BUG — a file with CRLF line endings does not compile:\n{printed}"
    );
}
