use super::items::*;
use super::Expr;
pub use crate::span::{Span, Spannable};

impl Spannable for Expr {
    fn span(&self) -> Span {
        self.content.span()
    }
}

impl Spannable for Log {
    fn span(&self) -> Span {
        match self {
            Log::Single(i) => i.span(),
            Log::And(l, r) | Log::Or(l, r) => {
                let from = l.span().0;
                let to = r.span().1;
                Span(from, to)
            }
        }
    }
}

impl Spannable for Equ {
    fn span(&self) -> Span {
        match self {
            Equ::Single(i) => i.span(),
            Equ::Equal(l, r) | Equ::NotEqual(l, r) => {
                let from = l.span().0;
                let to = r.span().1;
                Span(from, to)
            }
        }
    }
}

impl Spannable for Rel {
    fn span(&self) -> Span {
        match self {
            Rel::Single(i) => i.span(),
            Rel::LessEqual(l, r)
            | Rel::GreaterEqual(l, r)
            | Rel::LessThan(l, r)
            | Rel::GreaterThan(l, r) => {
                let from = l.span().0;
                let to = r.span().1;
                Span(from, to)
            }
        }
    }
}

impl Spannable for AddSub {
    fn span(&self) -> Span {
        match self {
            AddSub::Single(i) => i.span(),
            AddSub::Add(l, r) | AddSub::Sub(l, r) => {
                let from = l.span().0;
                let to = r.span().1;
                Span(from, to)
            }
        }
    }
}

impl Spannable for MulDiv {
    fn span(&self) -> Span {
        match self {
            MulDiv::Single(i) => i.span(),
            MulDiv::Mul(l, r) | MulDiv::Div(l, r) | MulDiv::Mod(l, r) => {
                let from = l.span().0;
                let to = r.span().1;
                Span(from, to)
            }
        }
    }
}

impl Spannable for Node {
    fn span(&self) -> Span {
        match self {
            Node::Single(i) => i.span(),
            Node::Plus(_, s) | Node::Minus(_, s) => s.clone(),
        }
    }
}

impl Spannable for Value {
    fn span(&self) -> Span {
        match self {
            Value::Single(i) => i.span(),
            Value::ArrElem(_, _, s) => s.clone(),
        }
    }
}

impl Spannable for Core {
    fn span(&self) -> Span {
        match self {
            Core::Str(_, s)
            | Core::Num(_, s)
            | Core::Ident(_, s)
            | Core::True(s)
            | Core::False(s)
            | Core::Paren(_, s)
            | Core::Arr(_, s) => s.clone(),
        }
    }
}
