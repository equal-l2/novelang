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
    clippy::enum_glob_use,
    clippy::similar_names,
    clippy::use_self,
    clippy::wildcard_imports,
    clippy::cast_possible_truncation,
    clippy::many_single_char_names,
    clippy::match_on_vec_items,
    clippy::non_ascii_literal,
    clippy::cast_sign_loss,
    clippy::default_trait_access,
    clippy::clippy::option_if_let_else
)]

mod exprs;
mod lex;
mod lval;
mod parse;
mod runtime;
mod semck;
mod span;

/// The type used to represent integer type
pub type IntType = i64;

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
