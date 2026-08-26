# CPLang — working agreement

CPLang is a compiler (Rust) for a terse competitive-programming language. It
compiles `.cpl` to C, which `gcc` then builds. See `README.md` for the language's
goals.

## Your role: tests only

**You write tests. You never touch non-test compiler code.**

- Writable: `src/tests/**`, `tests/**` and `src/test_derive/**`.
- Read-only: everything else — `src/compiler/**`, `src/core/**`, `src/main.rs`,
  `src/lib.rs`, `build.rs`, `Cargo.toml`. Read them freely to understand
  behaviour; never edit them, not even to fix a bug you just found.

Your two jobs:

1. Add coverage for features as they are requested.
2. Hunt for bugs by reading the compiler and probing it with cases.

**Every bug you report must come with a failing test that proves it.** A bug
without a red case is not a bug report, it is a guess. Before calling something a
bug, confirm the intended behaviour from the compiler source, `src/core/*.cpl`,
or existing cases — several "bugs" in this repo's history were misread semantics
and had to be reverted.

## Commands

```sh
cargo test                     # whole suite
cargo test test_08_structs     # one directory
cargo test -- --nocapture      # see compiler output / KNOWN BUG notes
cargo clippy --all-targets --all-features -- -D warnings   # CI denies warnings
```

CI (`.github/workflows`) runs build, `cargo test --all`, and clippy on Linux and
macOS. `gcc` must be on `PATH`. Built test binaries are cached in `.test_cache/`,
keyed by generated-C contents plus the gcc version.

## How the suite works

`src/tests/README.md` is the canonical description of the suite — read it first;
what follows is the short version.

Every `.cpl` file in a subdirectory of `src/tests/` is one test case.
`test_derive::generate_tests!` turns each into a `#[test]` named
`test_<dir>_<stem>`; `build.rs` fingerprints the file set so a newly added case
is not silently ignored. `src/tests/harness.rs` is the runner — read its module
doc before writing anything unusual.

A case declares its expectation in a header: the leading run of `//` lines.

| directive | meaning |
|---|---|
| `//OUT="text"` | must compile, run, and print exactly `text` |
| `//IN="text"` | fed to the program's stdin (`//OUT` cases only) |
| `//ERR l c l c` | must fail to compile, with an error at that span |
| `//ERR -1 -1 -1 -1` | must fail, with an error carrying no position |
| `//ERR any` | must fail; the span is not asserted |
| `//MSG="text"` | the error message must contain this (`//ERR` cases only) |
| `//BUG="text"` | the case is expected to be red until that bug is fixed |

Rules the harness enforces: exactly one of `//OUT` / `//ERR`; `//OUT` and `//IN`
may repeat and are concatenated in order; only `\n \t \r \0 \\ \"` are valid
escapes — an unknown one fails the case rather than quietly weakening it.
`//ERR` positions are the compiler's own: 0-based line and column counted over
the whole file *including the header*, end exclusive.

## Writing a case

- Directory names are ordered and topical (`00_lexical` … `12_stdlib`); put the
  case where the feature lives. `13_known_bugs` holds `//BUG` cases only.
- File names: `NN_snake_case_description.cpl`, numbered within the directory.
  A trailing `e` on the number (`69e_...`) marks a case that expects an error.
- Both the directory name and the stem must be `[A-Za-z0-9_]` — the proc macro
  pastes them into identifiers and rejects anything else.
- One behaviour per case. Prefer the smallest program that can fail for the
  reason you care about, so a failure names its own cause.
- Prefer an exact `//ERR l c l c` over `//ERR any`; use `any` when the span is
  genuinely not the point, or for a `//BUG` case whose future span is unknowable.
- Pair `//ERR` with `//MSG` so a case cannot pass on the wrong error.

## Bug cases

A `//BUG` case is written the way the language *should* behave, so it is red on
purpose — the suite doubles as the bug list. The harness catches panics so the
report stays readable, labels the failure `KNOWN BUG — <text>`, and prints a
note if the case ever starts passing (then drop the `//BUG` line and move the
file into the directory it belongs to).

Give the `//BUG` value a one-line description of the actual defect. Below the
header, write a comment explaining what the compiler does today, why it is
wrong, and where the fix would go — `src/tests/13_known_bugs/` and `main.cpl`
have examples. Some existing values cite `FEEDBACK.md`, a document kept outside
this repo; don't add new references to it.

## Compiler map (for reading, not editing)

`src/compiler/mod.rs` documents the pipeline; the stages are
preprocessor → tokenizer → macros → parser → lowerer → normalizer (name and
type resolution, AST → IR) → codegen (IR → C). The user's file is compiled
together with the core library in `src/core/*.cpl` (`operators`, `range`, `io`,
`vector`, `string`, `print`), so a "stdlib" bug may live in either place.
