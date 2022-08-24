use crate::types::IntType;

/// The typed content of a variable
#[derive(Debug, Clone)]
pub enum Val {
    Bool(bool),
    Num(IntType),
    Str(String),
    Sub(usize),
    Arr(Vec<Val>),
}

impl Val {
    pub const fn typename(&self) -> &'static str {
        match self {
            Self::Bool(_) => "Bool",
            Self::Num(_) => "Num",
            Self::Str(_) => "Str",
            Self::Sub(_) => "Sub",
            Self::Arr(_) => "Arr",
        }
    }
}

impl std::ops::Neg for Val {
    type Output = Self;
    fn neg(self) -> Self {
        match self {
            Self::Bool(b) => Self::Bool(!b),
            Self::Num(n) => Self::Num(-n),
            Self::Str(s) => Self::Str(s.chars().rev().collect()),
            Self::Arr(v) => Self::Arr(v.into_iter().rev().collect()),
            _ => unimplemented!(),
        }
    }
}

impl PartialEq for Val {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Val::Bool(this), Val::Bool(that)) => this.eq(that),
            (Val::Num(this), Val::Num(that)) => this.eq(that),
            (Val::Str(this), Val::Str(that)) => this.eq(that),
            (Val::Arr(this), Val::Arr(that)) => this.eq(that),
            _ => unimplemented!(),
        }
    }
}

impl PartialOrd for Val {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Val::Num(this), Val::Num(that)) => Some(this.cmp(that)),
            (Val::Str(this), Val::Str(that)) => Some(this.cmp(that)),
            _ => None,
        }
    }
}

impl std::fmt::Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Bool(b) => write!(f, "{}", b),
            Val::Num(n) => write!(f, "{}", n),
            Val::Str(s) => write!(f, "{}", s),
            Val::Sub(_) => unreachable!("must be denied at compile time"),
            Val::Arr(v) => {
                write!(f, "[")?;
                if !v.is_empty() {
                    write!(f, "{}", v[0])?;
                }
                for e in &v[1..] {
                    write!(f, ",{}", e)?;
                }
                write!(f, "]")?;
                Ok(())
            }
        }
    }
}
