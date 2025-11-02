#[cfg(test)]
mod tests {
    use test_derive::generate_tests;
    use crate::compiler::compile;
    use crate::compiler::error::FilePosition;
    use std::hash::Hasher;

    fn compile_gcc(c_file: &str) -> String {
        let cache_dir = "./.test_cache";
        std::fs::create_dir_all(cache_dir).unwrap();
        // hash contents of c_file to create unique file name (with only digit characters)
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        let contents = std::fs::read_to_string(&c_file).unwrap();
        hasher.write(contents.as_bytes());
        let hash = hasher.finish();
        let exec_file = format!("{}/test_exec_{}", cache_dir, hash);
        
        if std::path::Path::new(&exec_file).exists() {
            println!("Using cached executable");
            return exec_file;
        }
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

        exec_file
    }

    fn run_test(test_file: &str) {
        let c_file = test_file.replace(".cpl", ".c");

        let binding = std::fs::read_to_string(&test_file).unwrap();
        let first_line = binding.lines().next().unwrap();
        if first_line.starts_with("//ERR") {
            // split the line with space into 5 parts
            let parts: Vec<&str> = first_line.splitn(5, ' ').collect();
            // there are 4 integers, describing the error position
            let line_start: i32 = parts[1].parse().unwrap();
            let column_start: i32 = parts[2].parse().unwrap();
            let line_end: i32 = parts[3].parse().unwrap();
            let column_end: i32 = parts[4].parse().unwrap();

            let res = compile(&test_file, &c_file);

            if let Err(e) = res {
                if let Some(pos) = e.position {
                    assert_eq!(pos, FilePosition {
                        first_pos: (line_start as usize, column_start as usize),
                        last_pos: (line_end as usize, column_end as usize),
                    });
                } else {
                    if line_start != -1 {
                        panic!("Expected error position, but got None");
                    }
                }
            } else {
                std::fs::remove_file(&c_file).unwrap();
                panic!("Expected error, but got compilation success");
            }
        } else if first_line.starts_with("//OUT=") {
            let mut expected_output = first_line[6..].to_string();
            expected_output.push('\n');

            compile(&test_file, &c_file).unwrap();
            let exec_file = compile_gcc(&c_file);

            let output = std::process::Command::new(format!("./{exec_file}"))
                .output()
                .expect("failed to run test");

            std::fs::remove_file(&c_file).unwrap();

            assert_eq!(String::from_utf8(output.stdout).unwrap(), expected_output);
        } else {
            panic!("Invalid test file, first line must start with //ERR or //OUT=");
        }
    }

    generate_tests!();
}