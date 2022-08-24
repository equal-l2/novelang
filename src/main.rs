#![warn(
    future_incompatible,
    nonstandard_style,
    rust_2018_idioms,
    rust_2018_compatibility,
    rust_2021_compatibility,
    unused,
    clippy::pedantic,
    clippy::nursery
)]
#![allow(
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss,
    clippy::default_trait_access,
    clippy::enum_glob_use,
    clippy::many_single_char_names,
    clippy::match_on_vec_items,
    clippy::module_name_repetitions,
    clippy::needless_pass_by_value,
    clippy::non_ascii_literal,
    clippy::option_if_let_else,
    clippy::similar_names,
    clippy::use_self,
    clippy::wildcard_imports
)]

mod block;
mod exprs;
mod lex;
mod lval;
mod parse;
mod runtime;
mod semck;
mod span;
mod types;

pub use types::*;

#[macro_export]
macro_rules! die {
    ($( $x:expr ),*) => {
        {
            eprintln!($($x,)*);
            std::process::exit(1)
        }
    }
}

use clap::Parser;

#[derive(clap::Parser)]
struct Opt {
    filename: String,
}

#[derive(Debug)]
enum InputError<'a> {
    Stdin(std::io::Error),
    File(&'a str, std::io::Error),
}

impl<'a> std::fmt::Display for InputError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Stdin(e) => write!(f, "failed to read stdin ({})", e),
            Self::File(name, e) => write!(f, "failed to read \"{}\" ({})", name, e),
        }
    }
}

impl<'a> std::error::Error for InputError<'a> {}

fn read_input(filename: &str) -> Result<String, InputError<'_>> {
    if filename == "-" {
        let mut s = String::new();
        std::io::Read::read_to_string(&mut std::io::stdin(), &mut s).map_err(InputError::Stdin)?;
        Ok(s)
    } else {
        std::fs::read_to_string(filename).map_err(|e| InputError::File(filename, e))
    }
}

fn main() {
    env_logger::init();
    let opt = Opt::parse();
    let s = read_input(&opt.filename).unwrap_or_else(|e| die!("Input error: {}", e));

    eprintln!("Info: Lexing");
    let lexed = lex::lex(s).unwrap_or_else(|e| {
        eprintln!("Syntax Error: {}", e);
        die!("A syntax error was found, quitting...")
    });
    eprintln!("Lexed:\n{}", lexed);

    eprintln!("Info: Parsing");
    let parsed = parse::parse(&lexed).unwrap_or_else(|e| {
        let info = lexed.generate_error_mesg(e.1 .0);
        eprintln!("Error: {}\n{}", e.0, info);
        die!("A syntax error was found, quitting...")
    });
    eprintln!("{:?}", parsed.stmts);

    eprintln!("Info: Checking block syntax");
    let block_checked = block::check_block(parsed).unwrap_or_else(|es| {
        let len = es.len();
        for e in es {
            let info = lexed.generate_error_mesg(e.span.0);
            eprintln!("Error: {}\n{}", e.kind, info);
        }
        if len == 1 {
            die!("A block syntax error was found, quitting...")
        } else {
            die!("{} block syntax errors was found, quitting...", len)
        }
    });
    eprintln!("{:?}", block_checked.stmts);

    eprintln!("Info: Checking semantics");
    let final_insts = semck::check_semantics(block_checked).unwrap_or_else(|es| {
        let len = es.len();
        for e in es {
            let info = lexed.generate_error_mesg(e.span.0);
            eprintln!("Error: {}\n{}", e.kind, info);
        }
        if len == 1 {
            die!("A semantic error was found, quitting...")
        } else {
            die!("{} semantic errors was found, quitting...", len)
        }
    });
    eprintln!("{:?}", final_insts.stmts);
    eprintln!("Info: Load completed");

    runtime::run(final_insts);
}
