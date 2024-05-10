use crate::ast::GustFile;
use crate::compiler::transform::Transform;
use log::info;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;
use std::process::{Command, Stdio};

mod transform;

pub struct Compiler;

impl Compiler {
    pub fn new() -> Self {
        Self
    }

    pub fn compile(&self, filename: PathBuf, file: GustFile) -> anyhow::Result<()> {
        // transform code into go lang
        info!(
            "compiling {} into go lang",
            filename.file_name().unwrap().to_str().unwrap()
        );
        let content = format!("package main \n{}", file.to_go());

        let mut go_file = filename;
        go_file.set_extension("go");
        let go_file_name = go_file.file_name().unwrap().to_str().unwrap();
        info!("saving the content into golang file {}", go_file_name);

        std::fs::write(&go_file, content)?;
        // execute go build
        info!("executing file {}", go_file_name);

        let mut cmd = Command::new("go")
            .args(["run", go_file_name])
            .stdout(Stdio::piped())
            .spawn()
            .unwrap();
        {
            let stdout = cmd.stdout.as_mut().unwrap();
            let stdout_reader = BufReader::new(stdout);
            let stdout_lines = stdout_reader.lines();

            for line in stdout_lines {
                if let Ok(line) = line {
                    println!("{}", line);
                }
            }
        }

        cmd.wait().unwrap();
        Ok(())
    }
}
