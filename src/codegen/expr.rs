use super::Inst;
use crate::exprs::{items::*, Expr};

pub(super) trait Codegen {
    fn codegen(&self) -> Vec<Inst>;
}

impl Codegen for Expr {
    fn codegen(&self) -> Vec<Inst> {
        self.content.codegen()
    }
}

impl Codegen for Rel {
    fn codegen(&self) -> Vec<Inst> {
        match self {
            Self::Single(i) => i.codegen(),
            Self::Equal(l, r) => l
                .codegen()
                .into_iter()
                .chain(r.codegen().into_iter())
                .chain([Inst::Eq])
                .collect(),
            Self::NotEqual(l, r) => l
                .codegen()
                .into_iter()
                .chain(r.codegen().into_iter())
                .chain([Inst::Eq, Inst::Neg])
                .collect(),
            Self::LessEqual(l, r) => l
                .codegen()
                .into_iter()
                .chain(r.codegen().into_iter())
                .chain([Inst::GT, Inst::Neg])
                .collect(),
            Self::GreaterEqual(l, r) => l
                .codegen()
                .into_iter()
                .chain(r.codegen().into_iter())
                .chain([Inst::LT, Inst::Neg])
                .collect(),
            Self::LessThan(l, r) => l
                .codegen()
                .into_iter()
                .chain(r.codegen().into_iter())
                .chain([Inst::LT])
                .collect(),
            Self::GreaterThan(l, r) => l
                .codegen()
                .into_iter()
                .chain(r.codegen().into_iter())
                .chain([Inst::GT])
                .collect(),
        }
    }
}

impl Codegen for AddSub {
    fn codegen(&self) -> Vec<Inst> {
        match self {
            Self::Single(i) => i.codegen(),
            Self::Add(l, r) => l
                .codegen()
                .into_iter()
                .chain(r.codegen().into_iter())
                .chain([Inst::Add])
                .collect(),
            Self::Sub(l, r) => l
                .codegen()
                .into_iter()
                .chain(r.codegen().into_iter())
                .chain([Inst::Sub])
                .collect(),
        }
    }
}

impl Codegen for MulDiv {
    fn codegen(&self) -> Vec<Inst> {
        match self {
            Self::Single(i) => i.codegen(),
            Self::Mul(l, r) => l
                .codegen()
                .into_iter()
                .chain(r.codegen().into_iter())
                .chain([Inst::Mul])
                .collect(),
            Self::Div(l, r) => l
                .codegen()
                .into_iter()
                .chain(r.codegen().into_iter())
                .chain([Inst::Div])
                .collect(),
            Self::Mod(l, r) => l
                .codegen()
                .into_iter()
                .chain(r.codegen().into_iter())
                .chain([Inst::Mod])
                .collect(),
        }
    }
}

impl Codegen for Node {
    fn codegen(&self) -> Vec<Inst> {
        match self {
            Self::Single(i) => i.codegen(),
            Self::Plus(i) => i.codegen(),
            Self::Minus(i) => i
                .codegen()
                .into_iter()
                .chain([Inst::Neg])
                .collect(),
        }
    }
}

impl Codegen for Core {
    fn codegen(&self) -> Vec<Inst> {
        match self {
            Self::Str(i) => vec![Inst::Push(i.into())],
            Self::Num(i) => vec![Inst::Push((*i).into())],
            Self::Ident(i) => vec![Inst::Push(i.into()), Inst::Load],
            Self::True => vec![Inst::Push(true.into())],
            Self::False => vec![Inst::Push(false.into())],
            Self::Paren(i) => i.codegen(),
        }
    }
}
