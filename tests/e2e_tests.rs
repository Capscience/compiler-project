use compiler_project::compile;

use core::panic;
use std::{
    fs,
    io::{BufRead, Write},
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

const TESTFILE_DIRNAME: &str = "test_programs/";

#[test]
fn run_all_testcases() {
    let mut paths: Vec<PathBuf> = Vec::new();
    for file in fs::read_dir(Path::new(TESTFILE_DIRNAME)).expect("Failed to open test directory!") {
        let path = file.expect("Failed to open a test file!").path();
        paths.push(path);
    }
    let mut tests = E2ETests::new(paths);
    tests.run_all();
    tests.output();
}

struct E2ETests {
    paths: Vec<PathBuf>,
    results: Vec<Result<(), PathBuf>>,
}

impl E2ETests {
    pub fn new(paths: Vec<PathBuf>) -> Self {
        Self {
            paths,
            results: Vec::new(),
        }
    }

    pub fn run_all(&mut self) {
        for path in self.paths.clone() {
            self.run_testfile(&path);
        }
    }

    pub fn output(&self) {
        if self.results.len() != self.paths.len() {
            panic!(
                "{} out of {} tests run",
                self.results.len(),
                self.paths.len()
            );
        }
        let failures: Vec<Result<(), PathBuf>> = self
            .results
            .clone()
            .into_iter()
            .filter(move |result| result.is_err())
            .collect();
        if failures.is_empty() {
            return;
        }
        let mut output = format!(
            "E2E tests: {} passed; {} failed;\n\nfailures:\n",
            self.paths.len() - failures.len(),
            failures.len()
        );
        for case in failures {
            let path: PathBuf = case.unwrap_err();
            output.push_str(format!("{}\n", path.display()).as_str());
        }

        panic!("{}", output);
    }

    fn run_testfile(&mut self, path: &PathBuf) {
        let testfile = fs::read_to_string(path)
            .expect(format!("Failed to read testfile {}", path.to_string_lossy()).as_str());
        for case in testfile.split("---\n") {
            if self.run_testcase(case, path).is_err() {
                self.results.push(Err(path.to_path_buf()));
                let _ = fs::remove_file(Path::new("test.out"));
                return;
            }
        }
        let _ = fs::remove_file(Path::new("test.out"));
        self.results.push(Ok(()));
    }

    fn run_testcase(&mut self, case: &str, path: &PathBuf) -> Result<(), PathBuf> {
        let mut code = String::new();
        let mut outputs = Vec::new();
        let mut inputs = Vec::new();
        for line in case.lines() {
            if line.starts_with("prints ") {
                outputs.push(&line[7..]);
            } else if line.starts_with("input ") {
                inputs.push(format!("{}\n", line[6..].to_string()));
            } else {
                code.push_str(line);
                code.push('\n');
            }
        }

        compile(code, &Path::new("test.out"));
        let mut program = Command::new("./test.out")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("Failed to run ./test.out!");

        let mut stdin = program
            .stdin
            .take()
            .expect("Failed to open test program stdin!");

        for input in inputs {
            print!("{input}");
            stdin
                .write_all(input.as_bytes())
                .expect("Failed to write to test program stdin!");
        }

        let program_output = program.wait_with_output();

        if program_output.is_err() {
            return Err(path.to_path_buf());
        }

        for (i, line_result) in program_output.unwrap().stdout.lines().enumerate() {
            let line = line_result.unwrap();
            println!("Outputs:");
            println!("{line}");
            if outputs[i] != line {
                return Err(path.to_path_buf());
            }
        }
        Ok(())
    }
}
