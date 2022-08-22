use crate::exprs::Expr;
use crate::IdentName;

#[derive(Clone, Debug, derive_more::From)]
pub enum LVal {
    Scalar(IdentName),
    Vector(Box<Self>, Box<Expr>),
}

impl std::fmt::Display for LVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scalar(s) => write!(f, "{}", s),
            Self::Vector(l, r) => write!(f, "{}[{}]", l, r),
        }
    }
}
