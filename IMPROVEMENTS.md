# Improvements

Notes from a full read of the compiler (`src/compiler/**`), the core library
(`src/core/*.cpl`), the CLI and the tooling, followed by probing with real
programs and a mutation fuzzer over the existing cases.

Nothing outside the tests was changed. The bugs are recorded as red tests —
28 new cases in `src/tests/13_known_bugs/` (44–71) and three `known_bug_…`
tests in `tests/cli.rs` — and each of them explains its own defect. This file
is about the *designs* behind them, and the cleanups noticed along the way.

```sh
cargo test known_bug -- --nocapture   # the .cpl bug list
cargo test --test cli known_bug       # the three CLI ones
```

## 1. What the red tests cover

25 distinct defects. Cases that share a line share a root cause.

| cases | defect | where |
|---|---|---|
| 44, 45, 46 | a line comment swallows its newline, so the next line is glued onto it — a compile error at best, a statement silently moved into another block at worst | `preprocessor.rs` |
| 47 | a block comment at the start of a line counts as indentation | `preprocessor.rs` |
| 48 | tabs inside string and char literals become four spaces | `preprocessor.rs` |
| 49, 50 | operands and call arguments are evaluated in the C compiler's order — right to left with gcc | `codegen` |
| 51 | a field of a call result is treated as addressable; the C does not compile | `reference_check.rs` |
| 52, 54 | float constants are emitted without `.0` or an exponent, so they are C integers | `codegen` |
| 53e | a float literal beyond f64 becomes `inf` in the C | `tokenizer.rs`, `codegen` |
| 55 | i64 constants are emitted as C `int` | `codegen` |
| 56 | `print` of a float truncates instead of rounding (`4.1` → `4.099999999`) | `core/io.cpl` |
| 57 | `print` of a float ≥ 9.3e9 overflows an i64 and prints garbage | `core/io.cpl` |
| 58, 59 | the readers treat only space and `\n` as whitespace — CRLF and tabs break them | `core/io.cpl` |
| 60 | `Vec(n Vec(m 0))` makes n rows that share one buffer | `core/vector.cpl` |
| 61 | the core helper `is_whitespace` collides with a user function | `core/io.cpl` |
| 62 | a user macro is expanded inside the core library (compiler panic) | `macros.rs` |
| 63 | a bracketed argument to a core macro panics the position merge | `macros.rs`, `error.rs` |
| 64 | destructuring into a nested tuple | `lowerer` |
| 65 | a global initialised with a struct declared below it | `parser/mod.rs` |
| 66e | a variable is in scope inside its own first assignment (uninitialised read) | `normalizer` |
| 67e | duplicate parameter names are accepted | `parser/function.rs` |
| 68e | the type of an expression statement is never checked; `\|x` reaches gcc | `normalizer` |
| 69 | a method cannot call itself — nor can two methods call each other in a cycle | `normalizer` |
| 70 | a template function cannot call itself without explicit template arguments | `normalizer` |
| 71e | a format error's span ends in the wrong column after an escape | `parser/out.rs` |
| cli ×2 | `-o` naming the input file deletes or overwrites the source | `main.rs`, `compiler/mod.rs` |
| cli ×1 | a failed compile unlinks whatever `-o` names, including `/dev/null` as root | `main.rs` |

`30` was already there; its header described a cause that has since been
fixed (global initialisers are lowered now), so it was renamed and rewritten
for the cause that is left: an initialiser cannot see another global.

69 is the one to look at first. Recursive methods are what a union-find, a
segment tree or a graph struct consist of, and there is no workaround short
of moving the method out of the struct.

## 2. Design changes

Ordered roughly by how much each would remove.

### 2.1 Codegen leans on C for semantics the language has to own

Three unrelated bugs come from emitting an expression tree as one C
expression and trusting C to mean the same thing.

- **Evaluation order** (49, 50). Every operator is a call, and C does not
  order a call's arguments. gcc and clang disagree, so the same program gives
  different answers on different judges. Codegen should own the order: when
  more than one argument of a call (or field of a struct literal) can have a
  side effect, evaluate them into temporaries, one statement each, then call.
  An "is this expression pure" predicate over `IRExpression` keeps the common
  case — variables and constants — exactly as compact as it is now.
- **Constants carry no type** (52, 54, 55). `IRConstant::Int(i64)` and
  `IRConstant::Float(f64)` are printed with `{}`. The IR knows the type of
  every constant; the constant should carry it, and codegen should print
  `5LL`, `1.0`, `1e30`. Rejecting non-finite literals (53e) belongs in the
  tokenizer, next to the integer range checks.
- **`char` is C's `char`**, whose signedness is the platform's. `read_str`
  tests for end of input with `c != -1`. Built with `-funsigned-char`, which
  is what Linux on ARM does by default, `_builtin_getchar() == -1` is false
  at end of input, so that loop has no exit. Emit `signed char`, or give
  `_builtin_getchar` an `i32` result so that EOF is not squeezed into a char.

Two smaller things in the same file: a label directly before `}`
(`loop_cnt0:` at the end of a loop body) is only valid C from C23 on — emit
`loop_cnt0:;` — and `gen_block` re-indents by `replace('\n', "\n  ")` at
every nesting level, which is quadratic in depth for no functional gain.

### 2.2 The instance cache is consulted before the call is understood

`normalize_function` does, in this order: pad the template types with fresh
labels, look the call up in the cache, and only then unify the arguments
with the signature's hints. The cache compares with `are_equal`, which is
label identity in the union-find, so anything that is still an open label at
that moment — an auto-referenced receiver (69), a deduced template type (70)
— can never match. Recursion then instantiates until `RECURSION_LIMIT`.

Suggested shape:

1. `find_matching_function` already unifies arguments against the winning
   candidate and rolls back. Keep that unification for the winner instead of
   redoing it later, so the argument and template labels are final.
2. Look up the cache after that, keyed by the *signature* (an index into
   `functions_name_map`) rather than by the function's name, so overloads
   cannot be confused with each other.
3. Register an instance in the cache as soon as it is created, with its
   return label still open, and let a recursive call unify against that open
   label. A function with no `ret` settles as void at the end, as now.

Step 3 also removes the documented limitation that a recursive function needs
a return hint, or a `ret` above the recursive call (05_functions/40, 41, 55e)
— which today is why every DFS has to be written `-> void`.

While there: `find_matching_function` clones every candidate signature per
call and returns `functions_name_map[..][idx].clone()`, the whole body AST,
for every call site in the program. `Rc`, or indices into one table, would
take most of the allocation the `Cargo.toml` comment on mimalloc is about.

### 2.3 Indentation is measured after the text has been rewritten

`parse_strings_and_comments` rewrites the character stream — tabs to four
spaces, a block comment to one space, a line comment to nothing *including
its newline* — and `parse_indentation` then counts leading spaces of what is
left. Every rewrite therefore changes indentation (44–48).

Indentation is a property of the physical line. Compute it once, from the
raw characters of each line (tabs counted as four there and only there), keep
it beside the line, and let comment and string scanning produce fragments
without touching it. Tabs then survive inside literals for free. `\t`, `\r`
and `\0` are worth adding to `map_escape_char` at the same time; `char 9` is
currently the only way to write a tab.

### 2.4 Format strings are lexed a second time, by hand, in the parser

`parse_format_string` rebuilds fragments from the characters between `{` and
`}` and calls `parse_blocks`/`tokenize_fragments` itself, with positions
computed by arithmetic. That one decision is behind the existing 21, 42 and
43, behind 71e, behind macros not expanding inside a format (10_macros/54e),
and behind the empty-format panic in `tests/cli.rs`.

The preprocessor is the place that knows where a string starts, what its
escapes are and what position every character has. It could emit a format
string as a sequence of pieces — literal text, and for each `{…}` the
fragments inside it with their true positions — so that the ordinary
pipeline (character literals, nested strings, the stray-closer check, macro
expansion, real spans) applies to format expressions with no special code.

### 2.5 The user's file and the core library share every namespace

The six sources are concatenated into one token stream. Consequences:

- Macros are global (62): a user macro rewrites the core library. Expand each
  file with its own table, and let core export the macros it means to.
- Any un-prefixed core function is a reserved name (61). `is_whitespace` is
  the only accidental one; a per-file "private" marker, or a prefix
  convention checked by a test, would stop the next one.
- Generated names rely on characters the user "cannot type": `Method:name`,
  `2-tuple`, `$tmp3`. The last one can be typed — `$` is an ordinary
  identifier character — so `$tmp1 = 5` above a `for` loop fails with a type
  error naming `S0<i32, i32>`. Use a character the tokenizer rejects.
- A struct declared twice, in either file, is reported without a position.

### 2.6 `FilePosition` has a sentinel and an assert

`FilePosition::unknown()` is used as "no position" inside a type whose
`Option` already means that, so `Some(unknown())` exists, and
`display_error` reaches `unreachable!()` on it (existing CLI test; the
fuzzer kept finding new spellings — `5 * ( )`, `x: i32 ( ) = 5`, an empty
`{ }` in a string that spans lines).
`FilePosition::add` asserts that both sides are in the same file, which
turns a cosmetic operation into two panics (62, 63).

Make absence unrepresentable inside the type: positions are
`Option<FilePosition>`, joining is a function that returns the left side when
the files differ, and `TokenBlock` remembers the position of its own opening
bracket so that "expected another token" always has something to point at.
There are 96 uses of `unknown()` in `src/compiler` to migrate.

Seven errors are built with `position: None` although the place is known: a
mismatched bracket, a struct declared twice, a second `main`, `main`
returning a value, a field that does not exist (two sites) and field access
on a non-struct.

### 2.7 "Physical" is decided by the shape of one node

`is_expression_physical` says a `FieldAccess` is always addressable. It is
exactly as addressable as its base (51). The same function decides whether an
assignment target is legal, so `make().x = 5` passes the "non-assignable"
check too. Recursing into the base fixes both.

Related, not a bug by the suite's reading: a spilled temporary gets one
hidden variable per *expression site*, so `v.push(&(i * 2))` in a loop
stores the same address every iteration.

### 2.8 Names and types are checked only where a value is used

- A new variable is added to the symbol table before its initialiser is
  normalized (66e). Normalize the right-hand side first.
- `ASTStatement::Expression` drops its type label, so nothing ever asks the
  resolver whether the type exists (68e). Add it to `relevant_types`.
- Parameter and template names are not checked for duplicates (67e); struct
  fields are.
- A global's initialiser is normalized with no scope at all (30), and parsed
  before later structs are known (65). Treat the initialisers as the body of
  an implicit function that runs first: parsed with the function bodies,
  normalized inside `push_function`.

### 2.9 `Vec` is a value type that shares its storage

Copying a `Vec` copies `_size`, `_capacity` and the pointer. `b = a` gives
two vectors that see each other's element writes but not each other's pushes,
until one of them reallocates. 60 is where the standard library trips over
this itself; the same thing happens when a vector is passed to a function by
value (`v[0] = 9` is visible to the caller, `v.push(4)` is not).

Either make copies deep (a `clone` that the fill constructor and assignment
use), or make `Vec` a handle whose size and capacity live with the buffer so
that all copies agree. The current middle is the one option nobody expects.

The allocator has the matching problem: one 1 GiB `mmap`, never freed, with
no check on the `mmap` result or on running past the end. Every evaluation
of a string literal (`_Str_from_raw`) and every printed number (`print x:
i64` builds a `Str`) allocates, so a loop with constant live memory still
grows without bound. String literals could be views over static data, and
number printing could use a fixed buffer.

### 2.10 The CLI trusts its arguments

`-o` is written to, and on failure deleted, without looking at what it is
(three CLI tests). Refuse an output path that resolves to the input, and only
ever remove a regular file. Also:

- Compile errors go to stdout (`display_error` uses `println!`) while usage
  errors go to stderr. A driver that shows only stderr on failure shows
  nothing.
- `report_error` reads the source back from disk to draw the snippet. The
  compiler already had it in memory; returning it with the error removes a
  second read and the case where the file changed in between.
- `INPUT_NAMES` and `gain_input_sources` are parallel arrays of six that
  must be edited together, and the core sources are copied into `String`s on
  every compile and again on every error. One `const CORE: &[(&str, &str)]`
  of name and `include_str!` would do.
- `--help` and `-h` are taken as the input file name.

## 3. Recorded behaviour worth a second look

These are not bugs — the suite records each of them as intended — but they
are where a competitive programmer will lose time.

- **`&&` and `||` evaluate both sides** (03_operators/12). `i < n && v[i] ==
  x` reads out of bounds, and `x != 0 && 10 / x > 1` dies of SIGFPE. Lowering
  the bool/bool case to C's operators, and keeping the overloadable call for
  everything else, would fix the idiom without giving up overloading.
- **A range counts down when its end is below its start**
  (04_control_flow/12). `for i 0..n - 1` with `n == 0` therefore runs once,
  with `i == 0`, and `Vec(-3 0)` has three elements. An explicit spelling for
  descending ranges would let an empty ascending range stay empty.
- **Statements are not separated by newlines, and arguments not by commas.**
  `f(a -1)` is one argument; `f(a, -1)` is one tuple; both report "Function f
  does not exist." even though `f` exists with another arity. A line that
  begins with `-`, `(` or `[` continues the previous statement. At the least
  the arity error should name the candidates.
- **`cnt`, `brk`, `ret` and `out` are keywords**, and they are also the four
  names contestants use most for a counter, a flag and a result. `cnt = 0`
  fails with "Unexpected token" at the `=`.
- **Literals have one type.** `total: i64 = 0` is an error; it has to be `0L`.
- **A macro name shadows every identifier of that name** (10_macros/09).
- **The standard library skips edge cases by design** (README). One that was
  found and left out for that reason: `print` of the smallest i64 prints
  `-` followed by punctuation, because `x = -x` overflows.

## 4. Smaller cleanups

Preprocessor and tokenizer

- `newlines_to_spaces` clones each nested block to rewrite it
  (`*block = newlines_to_spaces(block.clone())`); take `&mut`.
- The decimal-point rule `curr_token.parse::<i32>().is_ok()` is behind the
  existing 27 and 36. A small number scanner — digits, optional `.digits`,
  optional exponent, optional `L` — replaces it and `string_to_token`'s
  chain of `parse` attempts.
- A token that is neither a number nor a valid name becomes an identifier:
  `9223372036854775808`, `1e5`, `a@b`. The user sees "Variable with name
  9223372036854775808 not found." Reject them where they are lexed.
- `Token::Semicolon` is commented `// :`.

Parser

- `structs: &Vec<ASTStructDeclaration>` is threaded through every parse
  function only to answer "is this identifier a struct name", by linear
  search. A `HashSet` of names collected in a first pass over the top-level
  tokens answers it, removes the two-phase dance in `parse_tokens` (including
  the `temp_structs` clone and the `pre_methods.pop()` that reverses method
  order), and fixes 65.
- `parse_expression` folds operators by scanning and `Vec::remove`-ing once
  per precedence level, and rebuilds the precedence table (seven `Vec`s) on
  every call. Precedence climbing with a `const fn precedence(op)` is shorter
  and linear.
- `parse_block` runs seven parsers in turn, each of which starts by peeking
  at the same token; a `match` on that token says the same thing directly.
- `parse_assignment(structs, expression.clone(), …)` clones the left-hand
  side of every statement, assignment or not.
- "Expected another token after this one" is written out 17 times. A
  `TokenBlock::expect_brace_block("after if condition")`-style helper would
  remove most of `statement.rs`.
- A unary operator's position is its operand's; the `-` or `!` itself is
  left out of every error span.

Normalizer and type resolver

- `normalize_function` saves and restores four fields with paired `swap`s,
  and every `?` in between skips the restore. A context struct pushed on a
  stack, or a guard, makes the pairing structural.
- `Rollback` "only works for a single save and then single revert" by
  comment. An assert on `rollback_mode` in `save_state` would make a nested
  save fail loudly instead of corrupting the resolver.
- `type_map: HashMap<IRType, IRTypeLabel>` is cloned by every `save_state`,
  which is once per overload candidate per call.
- Type errors print generated names (`S0<i32, i32>` for a `Range`) and
  resolver vocabulary ("-1-time reference"). The struct's source name and
  `&T` notation are available at that point.

Core library

- `operator<=` is `a < b || a == b`, and `operator>=` is `a > b || a == b`
  where `>` is itself `b < a`: several calls per comparison, with both sides
  of the `||` always evaluated.
- `Vec.pop` returns nothing, so reading the last element is a separate line.
- `Vec.expand` copies element by element through `operator[]` on every
  doubling.

## 5. Gaps in the standard library

Things a contest solution reaches for and has to write by hand today:
`read_f64` and `read_char`; `min`, `max`, `abs`, `swap`; a sort; `==` and
`<` on `Str`, and concatenation; `==` on `bool`; `char - char` (and
`char + char`, which 10_macros/11 adds as its example); ranges over `i64`;
`print` for a `Vec`; a way to print a float with a chosen number of decimals.

`getchar`/`putchar` per character is also the slow path on every libc;
`getchar_unlocked`/`putchar_unlocked`, or one buffer flushed at exit, is the
usual large win for input-heavy problems.

## 6. Tests and tooling

- **Red cases keep the whole suite red.** `cargo test` stops at the first
  failing target, so while any `//BUG` case exists `tests/cli.rs` does not
  run, and a new regression is one more failure among three dozen. Giving `//BUG` the
  usual expected-failure meaning — a case that fails counts as passed, one
  that passes fails the run and says the marker should come off — keeps the
  bug list and gets a green suite back.
- **CI only runs for `main`** (`.github/workflows/rust.yaml`); pushes to
  `dev` are not tested.
- **Generated C is only ever built at `-O0`.** The harness passes `-w` and
  nothing else, while `cses-tests/run.py` and every judge use `-O2`. Several
  findings here are undefined or unspecified behaviour in the generated C —
  argument order, an uninitialised local, the float-to-i64 overflow in
  `print` — which is exactly what changes between the two. A second pass of
  the suite at `-O2`, and a CI job with
  `-fsanitize=undefined,address`, would catch that class directly.
  `-funsigned-char` is a cheap way to test the other `char`.
- **A fuzzer is cheap here.** Mutating the existing cases (delete, duplicate
  or swap a line; replace a token) and flagging any compile that panics,
  overflows its stack, or succeeds with C that gcc rejects found 62, 68e and
  the many spellings of the unknown-position panic in 25,000 runs, a few
  minutes. "The compiler said yes and gcc said no" is a bug with no oracle
  needed.
- `cses-tests/run.py` runs solutions with no timeout, so one that loops
  hangs the script, and needs `requests`, which nothing declares.
- Stale references: `CLAUDE.md` lists `print` among the core files (it was
  folded into `io.cpl`) and points at a `main.cpl` that is git-ignored.
