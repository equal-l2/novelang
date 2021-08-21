/// The type used to represent integer type
pub type IntType = i64;

/// The typed content of a variable
#[derive(Debug, Clone)]
pub enum Typed {
    Bool(bool),
    Num(IntType),
    Str(String),
    Sub(usize),
    Arr(Vec<Typed>),
}

impl Typed {
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

impl std::ops::Neg for Typed {
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

impl PartialEq for Typed {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Typed::Bool(this), Typed::Bool(that)) => this.eq(that),
            (Typed::Num(this), Typed::Num(that)) => this.eq(that),
            (Typed::Str(this), Typed::Str(that)) => this.eq(that),
            (Typed::Arr(this), Typed::Arr(that)) => this.eq(that),
            _ => unimplemented!(),
        }
    }
}

impl PartialOrd for Typed {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Typed::Num(this), Typed::Num(that)) => Some(this.cmp(that)),
            (Typed::Str(this), Typed::Str(that)) => Some(this.cmp(that)),
            _ => None,
        }
    }
}

impl std::fmt::Display for Typed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Typed::Num(n) => write!(f, "{}", n),
            Typed::Bool(b) => write!(f, "{}", b),
            Typed::Str(s) => write!(f, "{}", s),
            Typed::Arr(v) => {
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
            _ => unimplemented!("unexpected type for print"),
        }
    }
}
