#[cfg(test)]
mod tests {
    use crate::compiler::compile;

    fn run_test(test_file: &str, expected_output: &str) {
        let c_file = test_file.replace(".cpl", ".c");
        let exec_file = test_file.replace(".cpl", "");
        compile(&format!("src/tests/{test_file}"), &c_file);
        let output = std::process::Command::new("gcc")
            .arg(&c_file)
            .arg("-o")
            .arg(&exec_file)
            .output()
            .expect("failed to compile test");

        if !output.status.success() {
            println!("{}", String::from_utf8(output.stderr).unwrap());
            panic!("failed to compile test");
        }
        let output = std::process::Command::new(format!("./{exec_file}"))
            .output()
            .expect("failed to run test");

        assert_eq!(String::from_utf8(output.stdout).unwrap(), expected_output);

        std::fs::remove_file(&c_file).unwrap();
        std::fs::remove_file(&exec_file).unwrap();
    }

    #[test]
    fn hello_world() {
        run_test("00_hello_world.cpl", "Hello, World!\n");
    }

    #[test]
    fn print_variable() {
        run_test("01_print_variable.cpl", "hello 10\n");
    }

    #[test]
    fn expression() {
        run_test("02_expression.cpl", "210\n");
    }

    #[test]
    fn print_expression() {
        run_test("03_print_expression.cpl", "230\n");
    }

    #[test]
    fn parentheses() {
        run_test("04_parentheses.cpl", "450\n");
    }

    #[test]
    fn function_call() {
        run_test("05_function_call.cpl", "60\n");
    }
}