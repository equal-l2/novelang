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

pub mod fuzz_utils;

pub enum Error {
    Lex(lex::Error),
    Parse(lex::Lexed, parse::Error),
    Block(lex::Lexed, Vec<block::Error>),
    Semck(lex::Lexed, Vec<semck::Error>),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lex(e) => write!(f, "Lex error: {}", e),
            Self::Parse(lexed, e) => {
                let info = lexed.generate_error_mesg(e.1 .0);
                write!(f, "Parse error: {}\n{}", e.0, info)
            }
            Self::Block(lexed, es) => {
                for e in es {
                    let info = lexed.generate_error_mesg(e.span.0);
                    write!(f, "Block syntax error: {}\n{}", e.kind, info)?;
                }
                Ok(())
            }
            Self::Semck(lexed, es) => {
                for e in es {
                    let info = lexed.generate_error_mesg(e.span.0);
                    write!(f, "Semantic error: {}\n{}", e.kind, info)?;
                }
                Ok(())
            }
        }
    }
}

pub use runtime::run;
pub use semck::Ast;

pub fn compile(s: &str) -> Result<Ast, Error> {
    let lexed = lex::lex(s).map_err(Error::Lex)?;
    let parsed = parse::parse(&lexed).map_err(|e| Error::Parse(lexed.clone(), e))?;
    let block_checked = block::check_block(parsed).map_err(|e| Error::Block(lexed.clone(), e))?;
    let final_insts =
        semck::check_semantics(block_checked).map_err(|e| Error::Semck(lexed.clone(), e))?;
    Ok(final_insts)
}
