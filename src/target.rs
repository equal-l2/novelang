use crate::exprs::Expr;
use crate::span::{Span, Spannable};
use crate::types::IdentName;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ident(pub IdentName, pub Span);

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Spannable for Ident {
    fn span(&self) -> Span {
        self.1.clone()
    }
}

impl From<Ident> for IdentName {
    fn from(i: Ident) -> Self {
        i.0
    }
}

// Values that can be targets for modification
#[derive(Clone, Debug)]
pub enum Target {
    Scalar(Ident),
    Vector(Box<Self>, Box<Expr>),
}

impl std::fmt::Display for Target {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scalar(i) => write!(f, "{}", i),
            Self::Vector(l, r) => write!(f, "{}[{}]", l, r),
        }
    }
}

impl Spannable for Target {
    fn span(&self) -> Span {
        match self {
            Self::Scalar(i) => i.span(),
            // Tail must be +1 to cover the closing bracket
            Self::Vector(l, r) => Span(l.span().0, r.span().1 + 1),
        }
    }
}

impl From<Ident> for Target {
    fn from(ident: Ident) -> Self {
        Self::Scalar(ident)
    }
}
