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
pub use lex::Token;
pub use runtime::run;
pub use semck::Ast;

use span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    pub row: usize,
    pub col: usize,
}

#[derive(Clone, Debug)]
pub struct Range(pub Location, pub Location);

pub enum Error {
    Lex(Vec<(lex::Error, Range)>),
    Parse(parse::Error, Range),
    Block(Vec<(block::Error, Range)>),
    Semck(Vec<(semck::Error, Range)>),
}

pub(crate) fn span_to_range(span: Span, tokens: &[Token]) -> Range {
    let head = tokens
        .get(span.0)
        .map_or_else(|| tokens.last().unwrap().next_col(), |tk| tk.loc.clone());
    let tail = tokens.get(span.1).map_or_else(
        || tokens.last().unwrap().next_col(),
        |tk| Location {
            row: tk.loc.row,
            col: tk.loc.col + tk.item.len(),
        },
    );
    Range(head, tail)
}

pub fn compile<S: AsRef<str>>(s: &[S]) -> Result<Ast, Error> {
    let lexed = lex::lex(s).map_err(|v| Error::Lex(v))?;
    let parsed =
        parse::parse(&lexed).map_err(|(e, s)| Error::Parse(e, span_to_range(s, &lexed.tokens)))?;
    let block_checked = block::check_block(parsed).map_err(|v| {
        Error::Block(
            v.into_iter()
                .map(|(e, s)| (e, span_to_range(s, &lexed.tokens)))
                .collect(),
        )
    })?;
    let final_insts = semck::check_semantics(block_checked).map_err(|v| {
        Error::Semck(
            v.into_iter()
                .map(|(e, s)| (e, span_to_range(s, &lexed.tokens)))
                .collect(),
        )
    })?;
    Ok(final_insts)
}
