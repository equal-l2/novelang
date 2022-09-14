use super::utils::parse_expr;
use super::LookItem;
use super::{Error, Result};
use crate::lex::{self, LangItem};
use crate::target::{Ident, Target};

pub fn parse_target<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<Target>
where
    T: Iterator<Item = (usize, &'a lex::Token)>,
{
    let (i, tk) = tks
        .peek()
        .ok_or_else(|| (Error("expected Ident".into()), last.into()))?;
    if let LangItem::Ident(name) = &tk.item {
        let mut val = Target::Scalar(Ident(name.clone(), (*i).into()));
        let _ = tks.next().unwrap();
        loop {
            if tks.peek().item() == Some(LangItem::LBra) {
                let _ = tks.next().unwrap();
                let expr = parse_expr(tks, last)?;
                expects!("expected RBra", LangItem::RBra, tks, last);
                val = Target::Vector(Box::from(val), Box::from(expr));
            } else {
                return Ok(val);
            }
        }
    } else {
        Err((Error("expected Ident".into()), (*i).into()))
    }
}
