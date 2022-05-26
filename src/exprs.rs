mod display;
pub mod items;
mod spannable;

use crate::span::Span;

#[derive(Clone, Debug)]
pub struct Expr {
    pub content: Box<items::TopItem>,
}

impl From<String> for Expr {
    fn from(s: String) -> Self {
        use items::*;
        Self {
            content: Box::from(Log::Single(Equ::Single(Rel::Single(AddSub::Single(
                MulDiv::Single(Node::Single(Value::Single(Core::Str(s, Span(0, 0))))),
            ))))),
        }
    }
}
