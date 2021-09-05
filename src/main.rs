#![warn(clippy::nursery)]
#![warn(clippy::pedantic)]
#![warn(future_incompatible)]
#![warn(rust_2018_compatibility)]
#![warn(rust_2018_idioms)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::enum_glob_use)]
#![allow(clippy::many_single_char_names)]
#![allow(clippy::match_on_vec_items)]
#![allow(clippy::non_ascii_literal)]
#![allow(clippy::similar_names)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::wildcard_imports)]

mod exprs;
mod lex;
mod lval;
mod parse;
mod runtime;
mod semck;
mod span;
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
        let mut s = String::new();
        std::io::Read::read_to_string(&mut std::io::stdin(), &mut s)
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
        Err(e) => {
            eprintln!("Syntax Error: {}", e);
            die!("A syntax error was found, quitting...")
        }
    };

    eprintln!("Info: Parsing");
    let parsed = parse::parse(&lexed).unwrap_or_else(|e| {
        let info = lexed.generate_error_mesg(e.1 .0);
        eprintln!("Error: {}\n{}", e.0, info);
        die!("A syntax error was found, quitting...")
    });
    eprintln!("{:?}", parsed.stmts);

    eprintln!("Info: Checking semantics");
    let ast = semck::check_semantics(parsed).unwrap_or_else(|es| {
        let len = es.len();
        for e in es {
            let info = lexed.generate_error_mesg(e.1 .0);
            eprintln!("Error: {}\n{}", e.0, info);
        }
        if len == 1 {
            die!("A semantic error was found, quitting...")
        } else {
            die!("{} semantic errors was found, quitting...", len)
        }
    });
    eprintln!("{:?}", ast.stmts);
    eprintln!("Info: Load completed");

    runtime::run(ast);
}
