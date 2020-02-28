mod parser;
mod runner;

#[derive(Debug, Clone)]
pub enum StmtType {
    Print { text: String },
    FnBegin { name: String, offset_to_end: usize },
    FnEnd,
    Call { name: String },
}

#[derive(Debug, Clone)]
pub struct Program {
    stmts: Vec<StmtType>,
    fns: std::collections::HashMap<String, usize>,
}

fn main() {
    let path = std::env::args().nth(1).unwrap_or_else(|| {
        eprintln!("File error: filename was not provided");
        std::process::exit(1);
    });
    let s = std::fs::read_to_string(path).unwrap_or_else(|e| {
        eprintln!("File error: file could not be read : {}", e);
        std::process::exit(1);
    });

    eprintln!("Info: loading the file");
    let parsed = parser::parse(s);
    eprintln!("Info: load completed");

    if let Some(i) = parsed {
        runner::run(i);
    } else {
        std::process::exit(1);
    }
}
