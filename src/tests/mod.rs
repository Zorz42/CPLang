#[cfg(test)]
#[allow(clippy::module_inception)]
mod tests {
    use crate::compiler::compile;
    use crate::compiler::error::{display_error, FilePosition};
    use std::hash::Hasher;
    use std::thread::sleep;
    use std::time::Duration;
    use test_derive::generate_tests;

    fn compile_gcc(c_file: &str) -> String {
        let cache_dir = "./.test_cache";
        std::fs::create_dir_all(cache_dir).unwrap();
        // hash contents of c_file to create unique file name (with only digit characters)
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        let contents = std::fs::read_to_string(c_file).unwrap();
        hasher.write(contents.as_bytes());
        let hash = hasher.finish();
        let exec_file = format!("{}/test_exec_{}", cache_dir, hash);

        if std::path::Path::new(&exec_file).exists() {
            return exec_file;
        }
        let output = std::process::Command::new("gcc")
            .arg(c_file)
            .arg("-o")
            .arg(&exec_file)
            .output()
            .expect("failed to compile test");

        if !output.status.success() {
            println!("{}", String::from_utf8(output.stderr).unwrap());
            panic!("failed to compile test");
        }

        exec_file
    }

    fn run_test(test_file: &str) {
        let c_file = test_file.replace(".cpl", ".c");

        let binding = std::fs::read_to_string(test_file).unwrap();
        let first_line = binding.lines().next().unwrap();
        if first_line.starts_with("//ERR") {
            // split the line with space into 5 parts
            let parts: Vec<&str> = first_line.splitn(5, ' ').collect();
            // there are 4 integers, describing the error position
            let line_start: i32 = parts[1].parse().unwrap();
            let column_start: i32 = parts[2].parse().unwrap();
            let line_end: i32 = parts[3].parse().unwrap();
            let column_end: i32 = parts[4].parse().unwrap();

            let res = compile(test_file, &c_file);

            if let Err(e) = res {
                if let Some(pos) = e.position.clone() {
                    let correct_pos = FilePosition {
                        first_pos: (line_start as usize, column_start as usize),
                        last_pos: (line_end as usize, column_end as usize),
                    };
                    if pos != correct_pos {
                        display_error(&e, &binding);
                        assert_eq!(pos, correct_pos);
                    }
                } else if line_start != -1 {
                    panic!("Expected error position, but got None");
                }
            } else {
                std::fs::remove_file(&c_file).unwrap();
                panic!("Expected error, but got compilation success");
            }
        } else if let Some(stripped) = first_line.strip_prefix("//OUT=") {
            let mut expected_output = stripped.to_string();
            expected_output.push('\n');

            if let Err(e) = compile(test_file, &c_file) {
                display_error(&e, &binding);
                panic!("compilation error")
            }
            let exec_file = compile_gcc(&c_file);

            let mut fails = 0;
            let output = loop {
                if let Ok(output) = std::process::Command::new(format!("./{exec_file}")).output() {
                    break output;
                } else {
                    sleep(Duration::from_millis(100));
                    fails += 1;
                    if fails == 10 {
                        panic!("Running test failed")
                    }
                }
            };

            std::fs::remove_file(&c_file).unwrap();

            assert_eq!(String::from_utf8(output.stdout).unwrap(), expected_output);
        } else {
            panic!("Invalid test file, first line must start with //ERR or //OUT=");
        }
    }

    generate_tests!();
}
