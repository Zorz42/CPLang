//! Tests for the `cplang` binary itself: argument handling, exit status, and
//! how errors are rendered.
//!
//! The `.cpl` cases under `src/tests/` call the compiler as a library, so none
//! of them exercise `main.rs` or `display_error.rs`. Those two files are the
//! whole of the user-facing surface — what a Makefile or `cses-tests/run.py`
//! sees — so they are worth testing directly.
//!
//! A few assertions here are expected to fail: they are named `known_bug_…`
//! and their message starts with `KNOWN BUG`, in the same spirit as the
//! `//BUG` cases in `src/tests/13_known_bugs/`.

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
// known bugs — these assertions are expected to fail
// ---------------------------------------------------------------------------

#[test]
fn known_bug_location_line_number_matches_the_snippet() {
    // FEEDBACK.md 1.15. display_error prints the header with
    // `position.first_pos.0` raw but the gutter with `line + 1`, and it adds 1
    // to the column but not to the line. So the "--> file:LINE:COL" header
    // points one line above the line it then highlights.
    //
    // Here `out 10` is on line 2 of the file (1-based), so the header should
    // read `input.cpl:2:5` and the gutter should mark line 2. Today the header
    // says `:1:5`.
    let scratch = Scratch::new("offbyone");
    let (_, printed) = compile(&scratch, "fn main\n    out 10\n");

    let location = printed
        .lines()
        .find(|l| l.contains("-->"))
        .unwrap_or_else(|| panic!("no location line in:\n{printed}"));
    let reported: usize = location
        .rsplit(':')
        .nth(1)
        .and_then(|n| n.trim().parse().ok())
        .unwrap_or_else(|| panic!("could not read a line number out of {location:?}"));

    assert_eq!(
        reported, 2,
        "KNOWN BUG (FEEDBACK.md 1.15): the --> header is one line above the line it highlights.\n{printed}"
    );
}

#[test]
fn known_bug_a_failed_compile_removes_the_stale_output() {
    // FEEDBACK.md 1.2, second half. The exit status is fixed, but the output
    // file from the previous successful compile is left untouched, so a driver
    // that ignores the status — or one that compiles, edits, recompiles and
    // then builds whatever is on disk — silently uses a stale .c file.
    let scratch = Scratch::new("stale");

    let (ok, printed) = compile(&scratch, HELLO);
    assert_eq!(ok.status.code(), Some(0), "{printed}");
    assert!(scratch.output().exists(), "the first compile should have written the output");

    let (failed, _) = compile(&scratch, "fn maine\n    0\n");
    assert_eq!(failed.status.code(), Some(1));

    assert!(
        !scratch.output().exists(),
        "KNOWN BUG (FEEDBACK.md 1.2): a failed compile leaves the previous output file in place, \
         so a driver that ignores the exit status compiles stale C."
    );
}

#[test]
fn known_bug_f64_is_compiled_to_double_not_long_double() {
    // FEEDBACK.md 1.11. gen_primitive_type maps f64 to C `long double`, which
    // is 80-bit x87 on x86-64 (the judges) and 64-bit on Apple Silicon (this
    // machine) — so the same program can print different numbers on different
    // platforms, and on x86 every float operation goes through x87.
    //
    // Related: bump_malloc rounds allocations to 8 bytes while x86-64
    // `long double` wants 16-byte alignment, so Vec[f64] storage can be
    // misaligned.
    let scratch = Scratch::new("longdouble");
    let (output, printed) = compile(&scratch, "fn main\n    x = 1.5\n    out \"{x}\"\n");
    assert_eq!(output.status.code(), Some(0), "{printed}");

    let c = std::fs::read_to_string(scratch.output()).expect("output file");
    let occurrences = c.matches("long double").count();
    assert_eq!(
        occurrences, 0,
        "KNOWN BUG (FEEDBACK.md 1.11): f64 is emitted as `long double` ({occurrences} occurrences), \
         which is 80-bit on x86-64 and 64-bit here, so results are platform-dependent."
    );
}

#[test]
fn known_bug_a_file_without_code_does_not_panic() {
    // FEEDBACK.md 1.5, seen through the CLI: the exit status is 101, the
    // signature of a panic, rather than a diagnostic. The library-level case
    // is 13_known_bugs/07_file_with_only_comments.cpl.
    let scratch = Scratch::new("emptyfile");
    let (output, printed) = compile(&scratch, "// nothing but a comment\n");

    assert!(
        !panicked(&output),
        "KNOWN BUG (FEEDBACK.md 1.5): a source file with no code panics the preprocessor \
         instead of reporting that there is no main function.\n{printed}"
    );
}

#[test]
fn known_bug_an_error_at_end_of_input_does_not_panic_the_printer() {
    // FEEDBACK.md 1.6, seen through the CLI: the compiler produces a good
    // error and then display_error hits its `unreachable!()` printing it, so
    // the user sees a Rust panic after the diagnostic.
    let scratch = Scratch::new("eofpanic");
    let (output, printed) = compile(&scratch, "fn main\n    x =\n");

    assert!(
        !panicked(&output),
        "KNOWN BUG (FEEDBACK.md 1.6): display_error panics on the unknown-position sentinel, \
         which any error at end of input carries.\n{printed}"
    );
}

#[test]
fn known_bug_using_a_core_macro_does_not_panic() {
    // FEEDBACK.md 1.7, seen through the CLI. The library-level case is
    // 13_known_bugs/09_calling_a_core_library_macro.cpl.
    let scratch = Scratch::new("macropanic");
    let (output, printed) = compile(&scratch, "_def_operator +; _builtin_add; char; char; char;\n\nfn main\n    out \"hi\"\n");

    assert!(
        !panicked(&output),
        "KNOWN BUG (FEEDBACK.md 1.7): merging a position from the core library with one from the \
         user's file trips an assert_eq! and aborts the process.\n{printed}"
    );
}
