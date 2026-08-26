//! Test harness for the `.cpl` files under `src/tests/`.
//!
//! Every `.cpl` file in a subdirectory of `src/tests/` is one test case;
//! `test_derive::generate_tests!` turns each of them into a `#[test]` function
//! that calls [`run_test`].
//!
//! What a case expects is written in its *header*: the leading run of `//`
//! comment lines at the top of the file. The recognised directives are
//!
//! | directive        | meaning                                                        |
//! |------------------|----------------------------------------------------------------|
//! | `//OUT="text"`   | the program must compile, run, and print exactly `text`        |
//! | `//IN="text"`    | `text` is fed to the program's stdin                           |
//! | `//ERR l c l c`  | compilation must fail with an error spanning that position     |
//! | `//ERR -1 -1 -1 -1` | compilation must fail with an error that carries no position |
//! | `//ERR any`      | compilation must fail; where is not asserted                   |
//! | `//MSG="text"`   | the error message must contain `text` (only with `//ERR`)      |
//! | `//BUG="text"`   | the case is expected to be red until the named bug is fixed     |
//!
//! A `//BUG` case is written the way the language *should* behave, so it fails
//! today on purpose — the suite doubles as the bug list. Its failure is
//! labelled `KNOWN BUG`, compiler panics are caught so the report stays
//! readable, and if it ever passes the harness says so instead.
//!
//! A case needs exactly one of `//OUT` and `//ERR`. `//OUT` and `//IN` may be
//! repeated, in which case the values are concatenated in order — that keeps
//! multi-line expectations readable:
//!
//! ```text
//! //OUT="1 2 3\n"
//! //OUT="4 5 6\n"
//! ```
//!
//! Inside the quotes, `\n`, `\t`, `\r`, `\0`, `\\` and `\"` are interpreted.
//! An unknown escape is a test-authoring error and fails the case, so a typo
//! can never quietly weaken an expectation.
//!
//! Positions in `//ERR` are the compiler's own: 0-based line and column,
//! counted over the whole file including the header, with the end exclusive.

use cplang::display_error;
use cplang::{FilePosition, compile};
use std::fmt::Write as _;
use std::hash::{DefaultHasher, Hasher};
use std::io::Write as _;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::OnceLock;
use std::sync::atomic::{AtomicU64, Ordering};

/// Flags handed to the C compiler. Part of the cache key, so changing them
/// here cannot silently reuse binaries built with the old ones.
const CC_FLAGS: [&str; 1] = ["-w"];

/// What the header of a test file asks for.
struct Expectation {
    stdin: String,
    kind: Kind,
    /// Set by `//BUG="…"`: the case describes what the compiler *should* do,
    /// and is expected to be red until the bug named here is fixed.
    bug: Option<String>,
}

enum Kind {
    /// Compiles, runs, and prints exactly this.
    Output(String),
    /// Fails to compile.
    Error { position: ExpectedPosition, message: Option<String> },
}

/// What an `//ERR` line says about where the error is reported.
enum ExpectedPosition {
    /// `//ERR l c l c` — exactly this span.
    Exact(FilePosition),
    /// `//ERR -1 -1 -1 -1` — the error must carry no position.
    Absent,
    /// `//ERR any` — it must fail, but where is not asserted. For cases whose
    /// span is not the point, and for `//BUG` cases that expect an error the
    /// compiler does not produce yet, whose eventual span cannot be known.
    Unchecked,
}

// ---------------------------------------------------------------------------
// header parsing
// ---------------------------------------------------------------------------

/// Interprets the escape sequences of a directive value.
fn unescape(value: &str) -> Result<String, String> {
    let mut res = String::new();
    let mut chars = value.chars();
    while let Some(c) = chars.next() {
        if c != '\\' {
            res.push(c);
            continue;
        }
        match chars.next() {
            Some('n') => res.push('\n'),
            Some('t') => res.push('\t'),
            Some('r') => res.push('\r'),
            Some('0') => res.push('\0'),
            Some('\\') => res.push('\\'),
            Some('"') => res.push('"'),
            Some(other) => return Err(format!("unknown escape sequence '\\{other}'")),
            None => return Err("trailing backslash".to_string()),
        }
    }
    Ok(res)
}

/// Strips the surrounding quotes of a directive value and unescapes it.
fn parse_quoted(directive: &str, rest: &str) -> Result<String, String> {
    let inner = rest
        .strip_prefix('"')
        .and_then(|r| r.strip_suffix('"'))
        .ok_or_else(|| format!("{directive} value must be wrapped in double quotes, got `{rest}`"))?;
    unescape(inner).map_err(|e| format!("{directive}: {e}"))
}

/// Parses what follows `//ERR`: four integers, or the word `any`.
fn parse_err_position(rest: &str) -> Result<ExpectedPosition, String> {
    let parts: Vec<&str> = rest.split_whitespace().collect();
    if parts == ["any"] {
        return Ok(ExpectedPosition::Unchecked);
    }
    if parts.len() != 4 {
        return Err(format!("//ERR takes 4 integers (line col line col) or `any`, got {}", parts.len()));
    }
    let mut nums = [0i64; 4];
    for (slot, part) in nums.iter_mut().zip(parts) {
        *slot = part.parse().map_err(|_| format!("//ERR: `{part}` is not an integer"))?;
    }

    if nums == [-1, -1, -1, -1] {
        return Ok(ExpectedPosition::Absent);
    }
    if nums.iter().any(|n| *n < 0) {
        return Err("//ERR: a position is either all four values or all -1".to_string());
    }
    Ok(ExpectedPosition::Exact(FilePosition {
        file_ident: 0,
        first_pos: (nums[0] as usize, nums[1] as usize),
        last_pos: (nums[2] as usize, nums[3] as usize),
    }))
}

/// Reads the directives out of the leading block of `//` comment lines.
fn parse_header(source: &str) -> Result<Expectation, String> {
    let mut output: Option<String> = None;
    let mut stdin = String::new();
    let mut error: Option<ExpectedPosition> = None;
    let mut message: Option<String> = None;
    let mut bug: Option<String> = None;

    for line in source.lines().take_while(|l| l.starts_with("//")) {
        let line = line.trim_end();
        if let Some(rest) = line.strip_prefix("//BUG=") {
            if bug.is_some() {
                return Err("duplicate //BUG directive".to_string());
            }
            bug = Some(parse_quoted("//BUG", rest)?);
        } else if let Some(rest) = line.strip_prefix("//OUT=") {
            output.get_or_insert_with(String::new).push_str(&parse_quoted("//OUT", rest)?);
        } else if let Some(rest) = line.strip_prefix("//IN=") {
            stdin.push_str(&parse_quoted("//IN", rest)?);
        } else if let Some(rest) = line.strip_prefix("//ERR") {
            if error.is_some() {
                return Err("duplicate //ERR directive".to_string());
            }
            error = Some(parse_err_position(rest)?);
        } else if let Some(rest) = line.strip_prefix("//MSG=") {
            if message.is_some() {
                return Err("duplicate //MSG directive".to_string());
            }
            message = Some(parse_quoted("//MSG", rest)?);
        }
        // any other `//` line in the header is an ordinary comment
    }

    match (output, error) {
        (Some(_), Some(_)) => Err("a test cannot expect both //OUT and //ERR".to_string()),
        (None, None) => Err("missing expectation: the header needs a //OUT=\"…\" or //ERR line".to_string()),
        (Some(output), None) => {
            if message.is_some() {
                return Err("//MSG only applies to //ERR tests".to_string());
            }
            Ok(Expectation {
                stdin,
                kind: Kind::Output(output),
                bug,
            })
        }
        (None, Some(position)) => {
            if !stdin.is_empty() {
                return Err("//IN only applies to //OUT tests".to_string());
            }
            Ok(Expectation {
                stdin,
                kind: Kind::Error { position, message },
                bug,
            })
        }
    }
}

// ---------------------------------------------------------------------------
// building and running
// ---------------------------------------------------------------------------

/// A generated `.c` file, kept out of the source tree and removed even when
/// the test that produced it panics.
struct GeneratedC {
    path: PathBuf,
}

impl GeneratedC {
    /// Names the file after the test that produces it, so a leftover file
    /// after a hard abort still says where it came from.
    fn for_test(label: &str) -> Self {
        static COUNTER: AtomicU64 = AtomicU64::new(0);

        let dir = std::env::temp_dir().join("cplang-tests");
        std::fs::create_dir_all(&dir).expect("could not create the temp directory for generated C");

        let stem: String = label
            .trim_end_matches(".cpl")
            .chars()
            .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
            .collect();
        let unique = COUNTER.fetch_add(1, Ordering::Relaxed);
        Self {
            path: dir.join(format!("{stem}_{}_{unique}.c", std::process::id())),
        }
    }

    fn as_str(&self) -> &str {
        self.path.to_str().expect("temp path is not valid UTF-8")
    }
}

impl Drop for GeneratedC {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.path);
    }
}

/// The C compiler's identity, folded into the cache key so that upgrading it
/// invalidates every cached binary instead of silently reusing stale ones.
fn cc_identity() -> &'static str {
    static IDENTITY: OnceLock<String> = OnceLock::new();
    IDENTITY.get_or_init(|| {
        let version = Command::new("gcc")
            .arg("--version")
            .output()
            .map(|o| String::from_utf8_lossy(&o.stdout).into_owned())
            .unwrap_or_default();
        format!("{version}{}", CC_FLAGS.join(" "))
    })
}

/// Compiles the generated C and returns the executable, reusing a cached
/// build when the same source has been compiled by the same compiler before.
fn compile_c(c_file: &Path) -> PathBuf {
    let cache_dir = Path::new(concat!(env!("CARGO_MANIFEST_DIR"), "/.test_cache"));
    std::fs::create_dir_all(cache_dir).expect("could not create .test_cache");

    let contents = std::fs::read(c_file).expect("generated C file disappeared");
    let mut hasher = DefaultHasher::new();
    hasher.write(&contents);
    hasher.write(cc_identity().as_bytes());
    let exec_file = cache_dir.join(format!("test_exec_{}", hasher.finish()));

    if exec_file.exists() {
        return exec_file;
    }

    // Build to a name unique to this call and rename into place. Two test
    // threads can reach here at once with byte-identical C — different cases
    // often compile to the same program — so they must not share a staging
    // file, and neither may ever observe a half-written binary.
    static STAGING_COUNTER: AtomicU64 = AtomicU64::new(0);
    let staging = exec_file.with_extension(format!("{}_{}.partial", std::process::id(), STAGING_COUNTER.fetch_add(1, Ordering::Relaxed)));
    let output = Command::new("gcc")
        .args(CC_FLAGS)
        .arg(c_file)
        .arg("-o")
        .arg(&staging)
        .output()
        .expect("could not run gcc — is it on PATH?");

    assert!(
        output.status.success(),
        "gcc rejected the generated C ({}):\n{}",
        c_file.display(),
        String::from_utf8_lossy(&output.stderr)
    );

    std::fs::rename(&staging, &exec_file).expect("could not move the compiled test binary into the cache");
    exec_file
}

/// Runs the built program and returns its stdout, or the reason it could not
/// be run to completion.
fn run_program(exec_file: &Path, stdin_data: &str) -> Result<String, String> {
    let mut child = Command::new(exec_file)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| format!("could not start {}: {e}", exec_file.display()))?;

    child
        .stdin
        .take()
        .expect("stdin was requested as a pipe")
        .write_all(stdin_data.as_bytes())
        .map_err(|e| format!("could not write to the program's stdin: {e}"))?;

    let output = child.wait_with_output().map_err(|e| format!("could not wait for the program: {e}"))?;

    match output.status.code() {
        Some(0) => {}
        Some(code) => {
            return Err(format!(
                "the program exited with status {code}\nstdout: {:?}\nstderr: {}",
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            ));
        }
        None => {
            return Err(format!(
                "the program was killed by a signal ({})\nstdout so far: {:?}",
                output.status,
                String::from_utf8_lossy(&output.stdout)
            ));
        }
    }

    String::from_utf8(output.stdout).map_err(|e| format!("the program printed invalid UTF-8: {e}"))
}

// ---------------------------------------------------------------------------
// the two kinds of case
// ---------------------------------------------------------------------------

/// Pretty-prints a compiler error, unless it carries the "unknown position"
/// sentinel — `display_error` reaches an `unreachable!()` on that value, and a
/// panic from the reporter would hide the failure it is meant to explain.
fn report(error: &cplang::CompilerError, case: &Case) {
    if error.position == Some(FilePosition::unknown()) {
        println!("Error: {} (at an unknown position)", error.message);
        return;
    }
    display_error(error, &case.path, &case.source);
}

fn check_output(case: &Case, expected: &str, stdin_data: &str) {
    let label = &case.label;
    let c_file = GeneratedC::for_test(label);

    if let Err(e) = compile(&case.path, c_file.as_str()) {
        report(&e, case);
        panic!("{label}: expected the program to compile, but it failed: {}", e.message);
    }

    let exec_file = compile_c(&c_file.path);
    let actual = match run_program(&exec_file, stdin_data) {
        Ok(stdout) => stdout,
        Err(reason) => panic!("{label}: {reason}"),
    };

    assert!(
        actual == expected,
        "{label}: wrong output\n  expected: {expected:?}\n    actual: {actual:?}{}",
        first_difference(expected, &actual)
    );
}

/// Points at the first differing byte, which is far easier to read than two
/// long escaped strings when the outputs are nearly identical.
fn first_difference(expected: &str, actual: &str) -> String {
    let common = expected.char_indices().zip(actual.chars()).take_while(|((_, e), a)| e == a).count();
    let mut res = String::new();
    let _ = write!(res, "\n  first difference at character {common}: ");
    match (expected.chars().nth(common), actual.chars().nth(common)) {
        (Some(e), Some(a)) => {
            let _ = write!(res, "expected {e:?}, got {a:?}");
        }
        (Some(e), None) => {
            let _ = write!(res, "expected {e:?}, but the output ended");
        }
        (None, Some(a)) => {
            let _ = write!(res, "expected the output to end, got {a:?}");
        }
        (None, None) => res.clear(),
    }
    res
}

fn check_error(case: &Case, expected_position: &ExpectedPosition, expected_message: Option<&str>) {
    let label = &case.label;
    let c_file = GeneratedC::for_test(label);

    let Err(error) = compile(&case.path, c_file.as_str()) else {
        panic!("{label}: expected a compile error, but the program compiled");
    };

    let show = |p: FilePosition| format!("{} {} {} {}", p.first_pos.0, p.first_pos.1, p.last_pos.0, p.last_pos.1);

    match (expected_position, error.position) {
        (ExpectedPosition::Unchecked, _) => {}
        (ExpectedPosition::Exact(expected), Some(actual)) if *expected != actual => {
            report(&error, case);
            panic!(
                "{label}: the error is reported at the wrong position\n  expected: {}\n    actual: {}",
                show(*expected),
                show(actual)
            );
        }
        (ExpectedPosition::Exact(expected), None) => panic!(
            "{label}: expected an error at {}, but the error carries no position: {}",
            show(*expected),
            error.message
        ),
        (ExpectedPosition::Absent, Some(actual)) => {
            report(&error, case);
            panic!("{label}: expected an error without a position, but it is reported at {}", show(actual));
        }
        (ExpectedPosition::Exact(_), Some(_)) | (ExpectedPosition::Absent, None) => {}
    }

    if let Some(expected) = expected_message {
        assert!(
            error.message.contains(expected),
            "{label}: wrong error message\n  expected it to contain: {expected:?}\n                  actual: {:?}",
            error.message
        );
    }
}

// ---------------------------------------------------------------------------
// entry point
// ---------------------------------------------------------------------------

/// One test case: the absolute path the generated test passes in, a short
/// name for failure messages, and the source text.
struct Case {
    path: String,
    label: String,
    source: String,
}

/// Runs one `.cpl` test case. Called by the functions `generate_tests!` emits.
pub fn run_test(test_file: &str) {
    let source = std::fs::read_to_string(test_file).unwrap_or_else(|e| panic!("could not read {test_file}: {e}"));

    // The generated tests pass absolute paths; failure messages read better
    // with the part that is the same for every case cut off.
    let label = test_file.split_once("src/tests/").map_or(test_file, |(_, rest)| rest).to_string();
    let case = Case {
        path: test_file.to_string(),
        label,
        source,
    };

    let expectation = match parse_header(&case.source) {
        Ok(expectation) => expectation,
        Err(problem) => panic!("{}: malformed test header: {problem}", case.label),
    };

    let Expectation { stdin, kind, bug } = expectation;
    let run = || match kind {
        Kind::Output(expected) => check_output(&case, &expected, &stdin),
        Kind::Error { position, message } => check_error(&case, &position, message.as_deref()),
    };

    let Some(bug) = bug else {
        run();
        return;
    };

    // A `//BUG` case states what the compiler *should* do, so it is red until
    // the bug is fixed. Catching the unwind is what makes the list readable:
    // several of these bugs are panics inside the compiler, and without this
    // the failure would be a raw `unreachable!()` or `index out of bounds`
    // with nothing tying it to the case that provoked it.
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(run)) {
        Ok(()) => println!(
            "NOTE: {} passes — the bug it was written for appears to be fixed.\n      Remove its //BUG line and move it into the directory it belongs to.",
            case.label
        ),
        Err(payload) => {
            let detail = payload
                .downcast_ref::<String>()
                .map(String::as_str)
                .or_else(|| payload.downcast_ref::<&str>().copied())
                .unwrap_or("<non-string panic payload>");
            panic!("KNOWN BUG — {bug}\n{detail}");
        }
    }
}

// ---------------------------------------------------------------------------
// tests for the harness itself
// ---------------------------------------------------------------------------

#[cfg(test)]
mod header_tests {
    use super::{ExpectedPosition, Kind, parse_header, unescape};

    #[test]
    fn interprets_escapes() {
        assert_eq!(unescape(r#"a\nb\tc\\d\"e\0f\r"#).unwrap(), "a\nb\tc\\d\"e\0f\r");
    }

    #[test]
    fn rejects_unknown_escapes() {
        assert!(unescape(r"\q").is_err());
        assert!(unescape(r"trailing\").is_err());
    }

    #[test]
    fn concatenates_repeated_directives() {
        let header = parse_header("//OUT=\"a\\n\"\n//OUT=\"b\"\n//IN=\"1 \"\n//IN=\"2\"\n\nfn main\n").unwrap();
        assert_eq!(header.stdin, "1 2");
        match header.kind {
            Kind::Output(out) => assert_eq!(out, "a\nb"),
            Kind::Error { .. } => panic!("expected an output expectation"),
        }
    }

    #[test]
    fn reads_error_positions() {
        match parse_header("//ERR 3 4 3 9\n").unwrap().kind {
            Kind::Error {
                position: ExpectedPosition::Exact(position),
                ..
            } => {
                assert_eq!(position.first_pos, (3, 4));
                assert_eq!(position.last_pos, (3, 9));
            }
            _ => panic!("expected an exact position"),
        }
        assert!(matches!(
            parse_header("//ERR -1 -1 -1 -1\n").unwrap().kind,
            Kind::Error {
                position: ExpectedPosition::Absent,
                ..
            }
        ));
        assert!(matches!(
            parse_header("//ERR any\n").unwrap().kind,
            Kind::Error {
                position: ExpectedPosition::Unchecked,
                ..
            }
        ));
    }

    /// `CompilerError` derives Debug and `FilePosition` writes `()` for itself,
    /// so that dumping an error while debugging the compiler does not bury the
    /// message under position data. Nothing in the compiler prints one, so
    /// this is the only thing that exercises it.
    #[test]
    fn a_compiler_error_can_be_formatted() {
        use cplang::{CompilerError, FilePosition};

        let positioned = CompilerError {
            message: "boom".to_string(),
            position: Some(FilePosition {
                file_ident: 0,
                first_pos: (1, 2),
                last_pos: (1, 5),
            }),
        };
        assert_eq!(format!("{positioned:?}"), "CompilerError { message: \"boom\", position: Some(()) }");

        let bare = CompilerError {
            message: "boom".to_string(),
            position: None,
        };
        assert_eq!(format!("{bare:?}"), "CompilerError { message: \"boom\", position: None }");
    }

    #[test]
    fn reads_the_bug_marker() {
        assert!(parse_header("//OUT=\"x\"\n").unwrap().bug.is_none());
        let header = parse_header("//BUG=\"FEEDBACK.md 1.1\"\n//OUT=\"x\"\n").unwrap();
        assert_eq!(header.bug.as_deref(), Some("FEEDBACK.md 1.1"));
        assert!(parse_header("//BUG=\"a\"\n//BUG=\"b\"\n//OUT=\"x\"\n").is_err());
    }

    #[test]
    fn reads_expected_message() {
        match parse_header("//ERR -1 -1 -1 -1\n//MSG=\"no main\"\n").unwrap().kind {
            Kind::Error { message, .. } => assert_eq!(message.as_deref(), Some("no main")),
            Kind::Output(_) => panic!("expected an error expectation"),
        }
    }

    #[test]
    fn stops_at_the_first_non_comment_line() {
        // The `//OUT` below is inside the program, not in the header.
        assert!(parse_header("fn main\n//OUT=\"x\"\n").is_err());
    }

    #[test]
    fn rejects_contradictory_or_missing_expectations() {
        assert!(parse_header("//OUT=\"a\"\n//ERR 1 1 1 1\n").is_err());
        assert!(parse_header("// just a comment\n").is_err());
        assert!(parse_header("//OUT=unquoted\n").is_err());
        assert!(parse_header("//ERR 1 2 3\n").is_err());
        assert!(parse_header("//ERR -1 2 3 4\n").is_err());
        assert!(parse_header("//OUT=\"a\"\n//MSG=\"x\"\n").is_err());
        assert!(parse_header("//ERR 1 1 1 1\n//IN=\"x\"\n").is_err());
    }
}
