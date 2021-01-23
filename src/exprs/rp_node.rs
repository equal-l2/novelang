use crate::lex::Ops;
use crate::types::IntType;

#[derive(Debug, Clone)]
pub enum RPNode {
    Bool(bool),
    Ident(String),
    Num(IntType),
    Ops(Ops),
}

impl RPNode {
    pub const fn typename(&self) -> &str {
        match self {
            Self::Bool(_) => "Bool",
            Self::Ident(_) => "Ident",
            Self::Num(_) => "Num",
            Self::Ops(_) => "Ops",
        }
    }
}
