use proc_macro::TokenStream;

fn create_code_for_test(name: String, file: String) -> String {
    let mut code = String::new();
    code.push_str("#[test]\n");
    code.push_str(&format!("fn {}() {{\n", name));
    code.push_str(&format!("    run_test(\"{file}\");\n"));
    code.push_str("}\n");
    code
}

// this macro will generate a bunch of test functions for every file in a directory
#[proc_macro]
pub fn generate_tests(_item: TokenStream) -> TokenStream {
    let dir = std::fs::read_dir("src/tests/").unwrap();

    let mut code = String::new();

    for entry in dir {
        let entry = entry.unwrap();
        let dir_path = entry.path();
        if dir_path.is_dir() {
            for file in std::fs::read_dir(&dir_path).unwrap() {
                let path = file.unwrap().path();
                let test_file = path.file_name().unwrap().to_str().unwrap();
                if test_file.ends_with(".cpl") {
                    let name = format!("test_{}_{}", dir_path.file_name().unwrap().to_str().unwrap(), test_file.replace(".cpl", ""));
                    let file = format!("{}/{}", dir_path.to_str().unwrap(), test_file);
                    code.push_str(&create_code_for_test(name, file));
                }
            }
        }
    }

    code.parse().unwrap()
}