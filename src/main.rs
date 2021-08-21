#![warn(clippy::nursery)]
#![warn(clippy::pedantic)]
#![warn(future_incompatible)]
#![warn(rust_2018_compatibility)]
#![warn(rust_2018_idioms)]
#![allow(clippy::enum_glob_use)]
#![allow(clippy::wildcard_imports)]
#![allow(clippy::similar_names)]
#![allow(clippy::many_single_char_names)]

mod exprs;
mod lex;
mod parse;
mod runtime;
mod types;

use structopt::StructOpt;

#[macro_export]
macro_rules! die {
    ($( $x:expr ),*) => {
        {
            eprintln!($($x,)*);
            std::process::exit(1)
        }
    }
}

#[derive(StructOpt)]
struct Opt {
    filename: String,
}

fn main() {
    env_logger::init();
    let opt = Opt::from_args();
    let s = if opt.filename == "-" {
        use std::io::Read;
        let mut s = String::new();
        std::io::stdin()
            .read_to_string(&mut s)
            .unwrap_or_else(|e| die!("Read error: failed to read stdin : {}", e));
        s
    } else {
        let name = &opt.filename;
        std::fs::read_to_string(name)
            .unwrap_or_else(|e| die!("Read error: failed to read file \"{}\" : {}", name, e))
    };

    eprintln!("Info: Lexing");
    let lexed = match lex::lex(s) {
        Ok(i) => {
            eprintln!("Lexed:\n{}", i);
            i
        }
        Err(e) => die!("Syntax Error: {}", e),
    };

    eprintln!("Info: Parsing");
    let parsed = parse::parse(lexed);
    eprintln!("{:?}", parsed.stmts);
    eprintln!("Info: Load completed");

    runtime::run(parsed);
}
