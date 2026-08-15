//! Turns every `.cpl` file under `src/tests/` into a `#[test]` function.

use proc_macro::TokenStream;
use std::path::{Path, PathBuf};

/// Rejects anything that would not survive being pasted into an identifier or
/// a string literal, so a stray file name produces a clear error here instead
/// of a confusing syntax error in the generated code.
fn is_usable_name(name: &str) -> bool {
    !name.is_empty() && name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
}

/// Collects `(directory, file stem)` for every test case, sorted, so the
/// generated code — and therefore the recompilation trigger — is stable.
fn discover_cases(tests_dir: &Path) -> Vec<(String, String)> {
    let mut cases = Vec::new();

    let dir = std::fs::read_dir(tests_dir).unwrap_or_else(|e| panic!("could not read {}: {e}", tests_dir.display()));
    for entry in dir {
        let entry = entry.expect("could not read a directory entry");
        let dir_path = entry.path();
        if !dir_path.is_dir() {
            continue;
        }
        let dir_name = dir_path.file_name().unwrap().to_str().expect("test directory name is not UTF-8").to_string();

        let files = std::fs::read_dir(&dir_path).unwrap_or_else(|e| panic!("could not read {}: {e}", dir_path.display()));
        for file in files {
            let path = file.expect("could not read a directory entry").path();
            let Some(file_name) = path.file_name().and_then(|n| n.to_str()) else {
                continue;
            };
            let Some(stem) = file_name.strip_suffix(".cpl") else {
                continue;
            };

            assert!(
                is_usable_name(&dir_name) && is_usable_name(stem),
                "test path `{}/{file_name}` must consist of letters, digits and underscores",
                dir_name
            );
            cases.push((dir_name.clone(), stem.to_string()));
        }
    }

    cases.sort();
    cases
}

/// Generates one `#[test]` per `.cpl` file under `src/tests/`.
///
/// The generated code also `include_bytes!`s every case. That value is never
/// read; it exists so cargo records the files as inputs of this crate and
/// rebuilds when one is added, changed or removed. Without it a new test file
/// silently does not run until something else forces a recompile.
#[proc_macro]
pub fn generate_tests(_item: TokenStream) -> TokenStream {
    // Resolve relative to the crate being compiled rather than the working
    // directory, which cargo does not promise to set to the crate root.
    let crate_root = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR is set by cargo"));
    let tests_dir = crate_root.join("src/tests");

    let cases = discover_cases(&tests_dir);
    assert!(!cases.is_empty(), "no .cpl test cases found in {}", tests_dir.display());

    let mut code = String::new();
    code.push_str("/// Forces cargo to treat the test files as inputs of this crate.\n");
    code.push_str("#[allow(dead_code)]\n");
    code.push_str("const TEST_FILE_CONTENTS: &[&[u8]] = &[\n");
    for (dir, stem) in &cases {
        code.push_str(&format!("    include_bytes!(concat!(env!(\"CARGO_MANIFEST_DIR\"), \"/src/tests/{dir}/{stem}.cpl\")),\n"));
    }
    code.push_str("];\n\n");

    for (dir, stem) in &cases {
        code.push_str("#[test]\n");
        code.push_str(&format!("fn test_{dir}_{stem}() {{\n"));
        code.push_str(&format!(
            "    run_test(concat!(env!(\"CARGO_MANIFEST_DIR\"), \"/src/tests/{dir}/{stem}.cpl\"));\n"
        ));
        code.push_str("}\n");
    }

    code.parse().expect("generated test code is not valid Rust")
}
