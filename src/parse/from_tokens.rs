pub(super) use crate::lex::{Keyword, LangItem, Token};
pub(super) use crate::parse::{Error, Expr, Result, utils::*};
pub(super) use crate::target::{Ident, Target};

pub(super) trait FromTokens {
    fn try_parse<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Self>
    where
        Self: Sized,
        T: Iterator<Item = (usize, &'a Token)>;
}
