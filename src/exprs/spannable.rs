use super::items::*;
use super::Expr;
pub use crate::span::{Span, Spannable};

impl Spannable for Expr {
    fn get_span(&self) -> Span {
        self.content.get_span()
    }
}

impl Spannable for Log {
    fn get_span(&self) -> Span {
        match self {
            Log::Single(i) => i.get_span(),
            Log::And(l, r) | Log::Or(l, r) => {
                let from = l.get_span().0;
                let to = r.get_span().1;
                Span(from, to)
            }
        }
    }
}

impl Spannable for Equ {
    fn get_span(&self) -> Span {
        match self {
            Equ::Single(i) => i.get_span(),
            Equ::Equal(l, r) | Equ::NotEqual(l, r) => {
                let from = l.get_span().0;
                let to = r.get_span().1;
                Span(from, to)
            }
        }
    }
}

impl Spannable for Rel {
    fn get_span(&self) -> Span {
        match self {
            Rel::Single(i) => i.get_span(),
            Rel::LessEqual(l, r)
            | Rel::GreaterEqual(l, r)
            | Rel::LessThan(l, r)
            | Rel::GreaterThan(l, r) => {
                let from = l.get_span().0;
                let to = r.get_span().1;
                Span(from, to)
            }
        }
    }
}

impl Spannable for AddSub {
    fn get_span(&self) -> Span {
        match self {
            AddSub::Single(i) => i.get_span(),
            AddSub::Add(l, r) | AddSub::Sub(l, r) => {
                let from = l.get_span().0;
                let to = r.get_span().1;
                Span(from, to)
            }
        }
    }
}

impl Spannable for MulDiv {
    fn get_span(&self) -> Span {
        match self {
            MulDiv::Single(i) => i.get_span(),
            MulDiv::Mul(l, r) | MulDiv::Div(l, r) | MulDiv::Mod(l, r) => {
                let from = l.get_span().0;
                let to = r.get_span().1;
                Span(from, to)
            }
        }
    }
}

impl Spannable for Node {
    fn get_span(&self) -> Span {
        match self {
            Node::Single(i) => i.get_span(),
            Node::Plus(_, s) | Node::Minus(_, s) => s.clone(),
        }
    }
}

impl Spannable for Value {
    fn get_span(&self) -> Span {
        match self {
            Value::Single(i) => i.get_span(),
            Value::ArrElem(_, _, s) => s.clone(),
        }
    }
}

impl Spannable for Core {
    fn get_span(&self) -> Span {
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
