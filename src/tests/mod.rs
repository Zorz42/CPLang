#[cfg(test)]
mod tests {
    use test_derive::generate_tests;
    use crate::compiler::compile;

    fn run_test(test_file: &str) {
        let c_file = test_file.replace(".cpl", ".c");
        let exec_file = test_file.replace(".cpl", "");

        let binding = std::fs::read_to_string(&test_file).unwrap();
        let first_line = binding.lines().next().unwrap();
        if !first_line.starts_with("//OUT=") {
            panic!("Invalid test file: first line must start with //OUT=");
        }
        let mut expected_output = first_line[6..].to_string();
        expected_output.push('\n');

        compile(&test_file, &c_file).unwrap();
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

    generate_tests!();
}