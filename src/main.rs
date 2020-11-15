mod exprs;
mod lex;
mod parse;
mod runtime;
mod types;

use structopt::StructOpt;

#[derive(StructOpt)]
struct Opt {
    filename: String,
}

fn main() {
    let opt = Opt::from_args();
    let s = if opt.filename == "-" {
        use std::io::Read;
        let mut s = String::new();
        std::io::stdin().read_to_string(&mut s).unwrap_or_else(|e| {
            eprintln!("Read error: failed to read stdin : {}", e);
            std::process::exit(1);
        });
        s
    } else {
        let name = &opt.filename;
        std::fs::read_to_string(name).unwrap_or_else(|e| {
            eprintln!("Read error: failed to read file \"{}\" : {}", name, e);
            std::process::exit(1);
        })
    };

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

    runtime::run(parsed);
}
