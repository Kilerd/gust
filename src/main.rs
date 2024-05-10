use crate::compiler::Compiler;
use anyhow::Result as AnyResult;
use log::error;
use std::path::PathBuf;

mod ast;
pub mod parser;

mod compiler;

fn main() -> AnyResult<()> {
    env_logger::init();
    let args: Vec<String> = std::env::args().collect();
    let compile_file = args.get(1);
    let Some(compile_file) = compile_file else {
        error!("no compile file");
        return Ok(());
    };
    let compile_file = PathBuf::from(compile_file);
    let file_content = std::fs::read_to_string(&compile_file)?;

    let span = crate::parser::Span::new(&file_content);
    let result = parser::parse_file(span).expect("cannot parse file");

    Compiler::new().compile(compile_file, result)?;
    Ok(())
}
