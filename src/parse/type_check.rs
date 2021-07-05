use super::{ScopeStack, Type};
use crate::exprs::{items::*, Expr};

pub(super) enum TypeError {
    VarNotFound(String),
    UnaryUndefined(Type),
    BinaryUndefined(Type, Type),
}

type Result = std::result::Result<Type, TypeError>;

pub(super) trait TypeCheck {
    fn check_type(&self, stack: &ScopeStack) -> Result;
}

impl TypeCheck for Expr {
    fn check_type(&self, stack: &ScopeStack) -> Result {
        self.content.check_type(stack)
    }
}

impl TypeCheck for Rel {
    fn check_type(&self, stack: &ScopeStack) -> Result {
        match self {
            Self::Single(i) => i.check_type(stack),
            Self::Equal(l, r)
            | Self::NotEqual(l, r)
            | Self::LessEqual(l, r)
            | Self::GreaterEqual(l, r)
            | Self::LessThan(l, r)
            | Self::GreaterThan(l, r) => {
                let l_ty = l.check_type(stack)?;
                let r_ty = r.check_type(stack)?;

                if l_ty == r_ty {
                    Ok(Type::Bool)
                } else {
                    Err(TypeError::BinaryUndefined(l_ty, r_ty))
                }
            }
        }
    }
}

impl TypeCheck for AddSub {
    fn check_type(&self, stack: &ScopeStack) -> Result {
        match self {
            Self::Single(i) => i.check_type(stack),
            Self::Add(l, r) => {
                let l_ty = l.check_type(stack)?;
                let r_ty = r.check_type(stack)?;

                if l_ty == r_ty && l_ty != Type::Sub {
                    Ok(l_ty)
                } else {
                    Err(TypeError::BinaryUndefined(l_ty, r_ty))
                }
            }
            Self::Sub(l, r) => {
                let l_ty = l.check_type(stack)?;
                let r_ty = r.check_type(stack)?;

                if l_ty == r_ty && l_ty == Type::Num {
                    Ok(l_ty)
                } else {
                    Err(TypeError::BinaryUndefined(l_ty, r_ty))
                }
            }
        }
    }
}

impl TypeCheck for MulDiv {
    fn check_type(&self, stack: &ScopeStack) -> Result {
        match self {
            Self::Single(i) => i.check_type(stack),
            Self::Mul(l, r) => {
                let l_ty = l.check_type(stack)?;
                let r_ty = r.check_type(stack)?;

                match (&l_ty, &r_ty) {
                    (Type::Num, Type::Num) => Ok(Type::Num),
                    (Type::Num, Type::Str) | (Type::Str, Type::Num) => Ok(Type::Str),
                    _ => Err(TypeError::BinaryUndefined(l_ty, r_ty)),
                }
            }
            Self::Div(l, r) | Self::Mod(l, r) => {
                let l_ty = l.check_type(stack)?;
                let r_ty = r.check_type(stack)?;

                if l_ty == r_ty && l_ty == Type::Num {
                    Ok(l_ty)
                } else {
                    Err(TypeError::BinaryUndefined(l_ty, r_ty))
                }
            }
        }
    }
}

impl TypeCheck for Node {
    fn check_type(&self, stack: &ScopeStack) -> Result {
        match self {
            Self::Single(i) => i.check_type(stack),
            Self::Plus(i) | Self::Minus(i) => {
                let ty = i.check_type(stack)?;
                if matches!(ty, Type::Num | Type::Str) {
                    Ok(ty)
                } else {
                    Err(TypeError::UnaryUndefined(ty))
                }
            }
        }
    }
}

impl TypeCheck for Core {
    fn check_type(&self, stack: &ScopeStack) -> Result {
        match self {
            Self::Str(_) => Ok(Type::Str),
            Self::Num(_) => Ok(Type::Num),
            Self::Ident(name) => stack
                .get_type_info(name)
                .map(|ti| ti.ty.clone())
                .ok_or_else(|| TypeError::VarNotFound(name.clone())),
            Self::True | Self::False => Ok(Type::Bool),
            Self::Paren(i) => i.check_type(stack),
        }
    }
}
