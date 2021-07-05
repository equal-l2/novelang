#![warn(future_incompatible)]
#![warn(rust_2018_compatibility)]
#![warn(rust_2018_idioms)]
#![warn(clippy::nursery)]
#![warn(clippy::pedantic)]
#![allow(clippy::fallible_impl_from)]
#![allow(clippy::future_not_send)]
#![allow(clippy::match_wildcard_for_single_variants)]
#![allow(clippy::needless_pass_by_value)]
#![allow(clippy::similar_names)]

mod codegen;
mod exprs;
mod lex;
mod parse;
mod runtime;
mod types;
mod vm;

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

    let asm = codegen::codegen(parsed.clone());
    eprintln!("{:?}", asm);

    runtime::run(parsed);
    vm::run(asm);
}
