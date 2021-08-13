pub use crate::exprs::Expr;

use crate::exprs::items::*;
use crate::types::Typed;

pub trait VarsMap {
    fn get(&self, name: &str) -> Option<&Typed>;
}

#[derive(Debug)]
pub enum EvalError {
    VariableNotFound(String),
    OverFlow,
    ZeroDivision,
    TypeError(String),
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Failed to eval because ")?;
        match self {
            Self::VariableNotFound(s) => write!(f, "variable {} was not found", s),
            Self::OverFlow => write!(f, "of overflow"),
            Self::ZeroDivision => write!(f, "of zero division"),
            Self::TypeError(s) => write!(f, "of type error: {}", s),
        }
    }
}

pub trait Eval {
    fn eval<T: VarsMap>(&self, vmap: &T) -> Result<Typed, EvalError>;
}

impl Eval for Expr {
    fn eval<T: VarsMap>(&self, vmap: &T) -> Result<Typed, EvalError> {
        self.content.eval(vmap)
    }
}

impl Eval for Log {
    fn eval<T: VarsMap>(&self, vmap: &T) -> Result<Typed, EvalError> {
        Ok(match self {
            Self::Single(l) => l.eval(vmap)?,
            Self::And(l, r) => {
                let l = l.eval(vmap)?;
                let r = r.eval(vmap)?;
                match (&l, &r) {
                    (Typed::Bool(l), Typed::Bool(r)) => Typed::Bool(*l && *r),
                    _ => Err(EvalError::TypeError(format!(
                        "cannot eval composite logic between {} with {}",
                        l.typename(),
                        r.typename()
                    )))?,
                }
            }
            Self::Or(l, r) => {
                let l = l.eval(vmap)?;
                let r = r.eval(vmap)?;
                match (&l, &r) {
                    (Typed::Bool(l), Typed::Bool(r)) => Typed::Bool(*l || *r),
                    _ => Err(EvalError::TypeError(format!(
                        "cannot eval composite logic between {} with {}",
                        l.typename(),
                        r.typename()
                    )))?,
                }
            }
        })
    }
}

impl Eval for Equ {
    fn eval<T: VarsMap>(&self, vmap: &T) -> Result<Typed, EvalError> {
        Ok(match self {
            Self::Single(l) => l.eval(vmap)?,
            Self::Equal(l, r) => {
                let l = l.eval(vmap)?;
                let r = r.eval(vmap)?;
                match (&l, &r) {
                    (Typed::Bool(l), Typed::Bool(r)) => Typed::Bool(l == r),
                    (Typed::Num(l), Typed::Num(r)) => Typed::Bool(l == r),
                    (Typed::Str(l), Typed::Str(r)) => Typed::Bool(l == r),
                    _ => Err(EvalError::TypeError(format!(
                        "cannot compare {} with {}",
                        l.typename(),
                        r.typename()
                    )))?,
                }
            }
            Self::NotEqual(l, r) => {
                let l = l.eval(vmap)?;
                let r = r.eval(vmap)?;
                match (&l, &r) {
                    (Typed::Bool(l), Typed::Bool(r)) => Typed::Bool(l != r),
                    (Typed::Num(l), Typed::Num(r)) => Typed::Bool(l != r),
                    (Typed::Str(l), Typed::Str(r)) => Typed::Bool(l != r),
                    _ => Err(EvalError::TypeError(format!(
                        "cannot compare {} with {}",
                        l.typename(),
                        r.typename()
                    )))?,
                }
            }
        })
    }
}

macro_rules! def_cmp {
    ($vmap: expr, $l: expr, $r: expr, $( $pat:pat )|+) => {
        {
            let l = $l.eval($vmap)?;
            let r = $r.eval($vmap)?;
            if let Some(o) = l.partial_cmp(&r) {
                Ok(Typed::Bool(matches!(o, $($pat)|+)))
            } else {
                Err(EvalError::TypeError(format!("cannot compare {} with {}", l.typename(), r.typename())))
            }
        }
    };
}

impl Eval for Rel {
    fn eval<T: VarsMap>(&self, vmap: &T) -> Result<Typed, EvalError> {
        use std::cmp::Ordering;
        Ok(match self {
            Self::Single(l) => l.eval(vmap)?,
            Self::LessEqual(l, r) => def_cmp!(vmap, l, r, Ordering::Less | Ordering::Equal)?,
            Self::GreaterEqual(l, r) => def_cmp!(vmap, l, r, Ordering::Greater | Ordering::Equal)?,
            Self::LessThan(l, r) => def_cmp!(vmap, l, r, Ordering::Less)?,
            Self::GreaterThan(l, r) => def_cmp!(vmap, l, r, Ordering::Greater)?,
        })
    }
}

macro_rules! def_ari {
    ($vmap: expr, $l: expr, $r: expr, $method: ident, $err: path, $op: literal) => {{
        let l = $l.eval($vmap)?;
        let r = $r.eval($vmap)?;
        match (&l, &r) {
            (Typed::Num(this), Typed::Num(that)) => match this.$method(*that) {
                Some(n) => Ok(Typed::Num(n)),
                None => Err($err),
            },
            _ => Err(EvalError::TypeError(format!(
                "cannot perform {} between {} and {}",
                $op,
                l.typename(),
                r.typename()
            ))),
        }
    }};
}

impl Eval for AddSub {
    fn eval<T: VarsMap>(&self, vmap: &T) -> Result<Typed, EvalError> {
        Ok(match self {
            Self::Single(l) => l.eval(vmap)?,
            Self::Add(l, r) => {
                let l = l.eval(vmap)?;
                let r = r.eval(vmap)?;
                match (&l, &r) {
                    (Typed::Num(this), Typed::Num(that)) => match this.checked_add(*that) {
                        Some(n) => Ok(Typed::Num(n)),
                        None => Err(EvalError::OverFlow),
                    },
                    (Typed::Str(this), Typed::Str(that)) => Ok(Typed::Str(this.clone() + that)),
                    _ => Err(EvalError::TypeError(format!(
                        "cannot perform {} between {} and {}",
                        "addition",
                        l.typename(),
                        r.typename()
                    ))),
                }
            }?,
            Self::Sub(l, r) => {
                def_ari!(vmap, l, r, checked_sub, EvalError::OverFlow, "subtraction")?
            }
        })
    }
}

impl Eval for MulDiv {
    fn eval<T: VarsMap>(&self, vmap: &T) -> Result<Typed, EvalError> {
        Ok(match self {
            Self::Single(l) => l.eval(vmap)?,
            Self::Mul(l, r) => {
                let l = l.eval(vmap)?;
                let r = r.eval(vmap)?;
                match (&l, &r) {
                    (Typed::Num(this), Typed::Num(that)) => match this.checked_mul(*that) {
                        Some(n) => Ok(Typed::Num(n)),
                        None => Err(EvalError::OverFlow),
                    },
                    (Typed::Num(n), Typed::Str(s)) | (Typed::Str(s), Typed::Num(n)) => {
                        Ok(Typed::Str(s.repeat(*n as usize)))
                    }
                    _ => Err(EvalError::TypeError(format!(
                        "cannot perform {} between {} and {}",
                        "multiplication",
                        l.typename(),
                        r.typename()
                    ))),
                }
            }?,
            Self::Div(l, r) => {
                def_ari!(vmap, l, r, checked_div, EvalError::ZeroDivision, "division")?
            }
            Self::Mod(l, r) => def_ari!(vmap, l, r, checked_rem, EvalError::ZeroDivision, "mod")?,
        })
    }
}

impl Eval for Node {
    fn eval<T: VarsMap>(&self, vmap: &T) -> Result<Typed, EvalError> {
        Ok(match self {
            Self::Single(l) => l.eval(vmap)?,
            Self::Plus(l) => l.eval(vmap)?,
            Self::Minus(l) => -l.eval(vmap)?,
        })
    }
}

impl Eval for Core {
    fn eval<T: VarsMap>(&self, vmap: &T) -> Result<Typed, EvalError> {
        Ok(match self {
            Self::Str(s) => Typed::Str(s.clone()),
            Self::Num(n) => Typed::Num(*n),
            Self::Ident(name) => vmap
                .get(name)
                .cloned()
                .ok_or_else(|| EvalError::VariableNotFound(name.clone()))?,
            Self::True => Typed::Bool(true),
            Self::False => Typed::Bool(false),
            Self::Paren(expr) => expr.eval(vmap)?,
        })
    }
}
