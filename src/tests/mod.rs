#[cfg(test)]
mod tests {
    use crate::compiler::compile;

    fn run_test(test_file: &str, expected_output: &str) {
        compile(&format!("src/tests/{test_file}"), "test.c");
        let output = std::process::Command::new("gcc")
            .arg("test.c")
            .arg("-o")
            .arg("test")
            .output()
            .expect("failed to compile test");

        assert!(output.status.success());

        let output = std::process::Command::new("./test")
            .output()
            .expect("failed to run test");

        assert_eq!(String::from_utf8(output.stdout).unwrap(), expected_output);

        std::fs::remove_file("test.c").unwrap();
        std::fs::remove_file("test").unwrap();
    }

    #[test]
    fn hello_world() {
        run_test("00_hello_world.cpl", "Hello, World!\n");
    }

    #[test]
    fn print_variable() {
        run_test("01_print_variable.cpl", "hello 10\n");
    }
}