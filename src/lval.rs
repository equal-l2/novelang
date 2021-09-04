use crate::exprs::Expr;

#[derive(Clone, Debug)]
pub enum LVal {
    Scalar(String),
    Vector(Box<Self>, Box<Expr>),
}

impl From<String> for LVal {
    fn from(s: String) -> Self {
        Self::Scalar(s)
    }
}

impl std::fmt::Display for LVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scalar(s) => write!(f, "{}", s),
            Self::Vector(l, r) => write!(f, "{}[{}]", l, r),
        }
    }
}
