pub(super) use crate::lex::{Keyword, LangItem, Token};
pub(super) use crate::parse::{utils::*, Error, Expr, Result};
pub(super) use crate::target::{Ident, Target};

pub(super) trait FromTokens {
    fn try_parse<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Self>
    where
        Self: Sized,
        T: Iterator<Item = (usize, &'a Token)>;
}
