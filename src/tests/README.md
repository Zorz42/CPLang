# The test suite

Every `.cpl` file in a subdirectory here is one end-to-end test: the compiler
runs over it, gcc builds the C it emits, the binary runs, and its output is
compared against what the file asks for. `test_derive::generate_tests!` turns
each file into a `#[test]` called `test_<directory>_<file stem>`, so

```
cargo test                              # everything
cargo test test_08_structs              # one directory
cargo test test_08_structs_27           # one case
```

`build.rs` and the `include_bytes!` the macro emits together make cargo notice
new, changed and deleted cases, so adding a file is enough — nothing else has
to be touched to make it run.

## Writing a case

The first lines of the file are its *header*: the leading run of `//` comments.
Directives in it say what the case expects; everything else there is an
ordinary comment, and a case that explains a subtlety should use one.

| directive           | meaning                                                       |
|---------------------|---------------------------------------------------------------|
| `//OUT="text"`      | the program must compile, run, and print exactly `text`       |
| `//IN="text"`       | `text` is fed to the program's stdin                          |
| `//ERR l c l c`     | compilation must fail with an error spanning that position    |
| `//ERR -1 -1 -1 -1` | compilation must fail with an error that carries no position  |
| `//ERR any`         | compilation must fail; where is not asserted                  |
| `//MSG="text"`      | the error message must contain `text` (goes with `//ERR`)     |
| `//BUG="text"`      | the case is red until the named bug is fixed — see below      |

`\n`, `\t`, `\r`, `\0`, `\\` and `\"` are interpreted inside the quotes; an
unknown escape fails the case rather than being taken literally. `//OUT` and
`//IN` may be repeated and are concatenated, which keeps multi-line
expectations readable:

```
//OUT="1 2 3\n"
//OUT="4 5 6\n"
```

`//ERR` positions are the compiler's own: 0-based line and column over the
whole file, header included, with the end exclusive.

Two habits keep the suite trustworthy. Give every `//ERR` case a `//MSG` — a
position alone passes for any error that happens to land there. And when a case
records behaviour that is surprising or known to be wrong, say so in a comment
in the header, so the next person knows whether a failure is a regression or a
fix.

## Red tests

`13_known_bugs/` is the bug list, written as tests. Each case is spelled the way
the language *should* behave and carries a `//BUG="…"` line, so it fails today
on purpose:

```
cargo test known_bug           # just the bug list
cargo test -- --skip known_bug # everything that should be green
```

A failing `//BUG` case is reported as `KNOWN BUG — <reason>`, and compiler
panics are caught so one bug cannot bury the rest of the report. If such a case
starts passing, the harness prints a note saying the bug looks fixed and the
marker should come off. Every one of them explains in its header what goes
wrong, why, and where in the compiler — most quote the section of `FEEDBACK.md`
they come from.

`tests/cli.rs` covers the binary itself — argument parsing, exit status and how
errors are rendered — and holds a few red assertions of its own, named
`known_bug_…`, for bugs that are only visible from outside the process.

## Coverage

`cargo llvm-cov --ignore-run-fail --summary-only` reports the compiler at about
98.5% of lines and 99.6% of functions. What is left is almost all
`unreachable!()` arms guarding compiler invariants — AST shapes the lowerer has
already removed, exhaustive-match tails, and a `panic!()` in `parse_indentation`
whose two conditions cannot both hold.

The rest is worth knowing about if you touch these files, because a test cannot
currently reach them: a handful of early rejections in
`type_resolver/compare_sets.rs`, `Dsu::merge` on two labels already in the same
component, the "two different struct types" error in `type_resolver` (the
generic type-conflict message always fires first), "Multiple main functions
found" in the normalizer (the duplicate-signature check gets there first),
restoring an overridden binding in `symbol_table`, `\t` escaping in
`codegen` (nothing can put a tab in a string — tabs become spaces before strings
are scanned), and three I/O and thread failures that a test cannot provoke.

## Layout

| directory         | what it covers                                                |
|-------------------|---------------------------------------------------------------|
| `00_lexical`      | comments, indentation and braces, literals, escapes           |
| `01_out`          | the `out` statement and format strings                        |
| `02_variables`    | locals, globals, scope, compound assignment                   |
| `03_operators`    | arithmetic, comparison, logic, precedence, casts, overloading |
| `04_control_flow` | `if`/`else`, `while`, `for`, ranges, nesting, `brk`/`cnt`      |
| `05_functions`    | calls, recursion, returns, overload resolution                |
| `06_templates`    | function templates and explicit template arguments            |
| `07_references`   | `&` and `\|`                                                  |
| `08_structs`      | fields, methods, struct templates, destructuring              |
| `09_tuples`       | tuple values, types and destructuring                         |
| `10_macros`       | macro declaration, expansion and their limits                 |
| `11_builtins`     | the `_builtin_*` primitives and containers built on them      |
| `12_stdlib`       | `Vec`, `Str`, `Range`, `print`, `read_*`, whole programs      |

Within a directory, cases that must compile are numbered from `00` and cases
that must fail are numbered from `50` and carry an `e` after the number.
