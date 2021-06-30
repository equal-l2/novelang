/// The type used to represent integer type
pub type IntType = i64;

/// The typed content of a variable
#[derive(Debug, Clone)]
pub enum Typed {
    Num(IntType),
    Bool(bool),
    Str(String),
    Sub(usize),
}

impl Typed {
    pub const fn typename(&self) -> &'static str {
        match self {
            Self::Num(_) => "Num",
            Self::Bool(_) => "Bool",
            Self::Str(_) => "Str",
            Self::Sub(_) => "Sub",
        }
    }
}

impl std::ops::Neg for Typed {
    type Output = Self;
    fn neg(self) -> Self {
        match self {
            Self::Num(n) => Self::Num(-n),
            Self::Bool(b) => Self::Bool(!b),
            Self::Str(s) => Self::Str(s.chars().rev().collect()),
            Self::Sub(_) => unimplemented!(),
        }
    }
}

impl PartialEq for Typed {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Typed::Num(this), Typed::Num(that)) => this.eq(that),
            (Typed::Bool(this), Typed::Bool(that)) => this.eq(that),
            (Typed::Str(this), Typed::Str(that)) => this.eq(that),
            _ => unimplemented!(),
        }
    }
}

impl PartialOrd for Typed {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Typed::Num(this), Typed::Num(that)) => Some(this.cmp(that)),
            _ => None,
        }
    }
}
