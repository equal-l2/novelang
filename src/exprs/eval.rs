use super::items::*;
use super::EvalError;
use crate::types::Typed;

pub trait VarsMap {
    fn get(&self, name: &str) -> Option<&Typed>;
}

pub trait Eval {
    fn eval_on<T: VarsMap>(&self, vmap: &T) -> Result<Typed, EvalError>;
}

macro_rules! def_cmp {
    ($vmap: expr, $l: expr, $r: expr, $( $pat:pat )|+) => {
        {
            let l = $l.eval_on($vmap)?;
            let r = $r.eval_on($vmap)?;
            if let Some(o) = l.partial_cmp(&r) {
                Ok(Typed::Bool(matches!(o, $($pat)|+)))
            } else {
                Err(EvalError::TypeError(format!("cannot compare {} with {}", l.typename(), r.typename())))
            }
        }
    };
}

impl Eval for Rel {
    fn eval_on<T: VarsMap>(&self, vmap: &T) -> Result<Typed, EvalError> {
        use std::cmp::Ordering;
        Ok(match self {
            Self::Single(l) => l.eval_on(vmap)?,
            Self::Equal(l, r) => def_cmp!(vmap, l, r, Ordering::Equal)?,
            Self::NotEqual(l, r) => def_cmp!(vmap, l, r, Ordering::Less | Ordering::Greater)?,
            Self::LessEqual(l, r) => def_cmp!(vmap, l, r, Ordering::Less | Ordering::Equal)?,
            Self::GreaterEqual(l, r) => def_cmp!(vmap, l, r, Ordering::Greater | Ordering::Equal)?,
            Self::LessThan(l, r) => def_cmp!(vmap, l, r, Ordering::Less)?,
            Self::GreaterThan(l, r) => def_cmp!(vmap, l, r, Ordering::Greater)?,
        })
    }
}

macro_rules! def_ari {
    ($vmap: expr, $l: expr, $r: expr, $method: ident, $err: path, $op: literal) => {{
        let l = $l.eval_on($vmap)?;
        let r = $r.eval_on($vmap)?;
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
    fn eval_on<T: VarsMap>(&self, vmap: &T) -> Result<Typed, EvalError> {
        Ok(match self {
            Self::Single(l) => l.eval_on(vmap)?,
            Self::Add(l, r) => {
                let l = l.eval_on(vmap)?;
                let r = r.eval_on(vmap)?;
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
    fn eval_on<T: VarsMap>(&self, vmap: &T) -> Result<Typed, EvalError> {
        Ok(match self {
            Self::Single(l) => l.eval_on(vmap)?,
            Self::Mul(l, r) => {
                let l = l.eval_on(vmap)?;
                let r = r.eval_on(vmap)?;
                match (&l, &r) {
                    (Typed::Num(this), Typed::Num(that)) => match this.checked_add(*that) {
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
    fn eval_on<T: VarsMap>(&self, vmap: &T) -> Result<Typed, EvalError> {
        Ok(match self {
            Self::Single(l) => l.eval_on(vmap)?,
            Self::Plus(l) => l.eval_on(vmap)?,
            Self::Minus(l) => -l.eval_on(vmap)?,
        })
    }
}

impl Eval for Core {
    fn eval_on<T: VarsMap>(&self, vmap: &T) -> Result<Typed, EvalError> {
        Ok(match self {
            Self::Str(s) => Typed::Str(s.clone()),
            Self::Num(n) => Typed::Num(*n),
            Self::Ident(name) => vmap
                .get(name)
                .cloned()
                .ok_or_else(|| EvalError::VariableNotFound(name.clone()))?,
            Self::True => Typed::Bool(true),
            Self::False => Typed::Bool(false),
            Self::Paren(expr) => expr.eval_on(vmap)?,
        })
    }
}
