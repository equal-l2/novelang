mod exprs;
mod lex;
mod parse;
mod run;

use structopt::StructOpt;

#[derive(StructOpt)]
struct Opt {
    filename: String,
}

fn main() {
    let opt = Opt::from_args();
    let s = std::fs::read_to_string(opt.filename).unwrap_or_else(|e| {
        eprintln!("File error: file could not be read : {}", e);
        std::process::exit(1);
    });

    eprintln!("Info: Lexing");
    let lexed = lex::lex(s).unwrap();
    eprintln!("Lexed:\n{}", lexed);
    eprintln!("Info: Parsing");
    let parsed = parse::parse(lexed);
    eprintln!("Info: Load completed");

    run::run(parsed);
}
