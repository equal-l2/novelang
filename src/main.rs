mod exprs;
mod lex;
mod parse;
mod run;
mod types;

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
    let lexed = match lex::lex(s) {
        Ok(i) => {
            //eprintln!("Lexed:\n{}", i);
            i
        }
        Err(e) => {
            eprintln!("Syntax Error: {}", e);
            std::process::exit(1);
        }
    };

    eprintln!("Info: Parsing");
    let parsed = parse::parse(lexed);
    eprintln!("{:?}", parsed.insts);
    eprintln!("Info: Load completed");

    run::run(parsed);
}
