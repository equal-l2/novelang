use super::{ScopeStack, Type};
use crate::exprs::{items::*, Expr};

pub(super) trait TypeCheck {
    fn check_type(&self, stack: &ScopeStack) -> Type;
}

impl TypeCheck for Expr {
    fn check_type(&self, stack: &ScopeStack) -> Type {
        self.content.check_type(stack)
    }
}

fn compare_types(l: Type, r: Type) -> Type {
    if l == Type::NotFound || r == Type::NotFound {
        Type::NotFound
    } else if l == r {
        l
    } else {
        Type::Conflict
    }
}

impl TypeCheck for Rel {
    fn check_type(&self, stack: &ScopeStack) -> Type {
        match self {
            Self::Single(i) => i.check_type(stack),
            Self::Equal(l, r)
            | Self::NotEqual(l, r)
            | Self::LessEqual(l, r)
            | Self::GreaterEqual(l, r)
            | Self::LessThan(l, r)
            | Self::GreaterThan(l, r) => {
                let l_ty = l.check_type(stack);
                let r_ty = r.check_type(stack);

                let cmpd = compare_types(l_ty, r_ty);
                if matches!(cmpd, Type::NotFound | Type::Conflict) {
                    cmpd
                } else {
                    Type::Bool
                }
            }
        }
    }
}

impl TypeCheck for AddSub {
    fn check_type(&self, stack: &ScopeStack) -> Type {
        match self {
            Self::Single(i) => i.check_type(stack),
            Self::Add(l, r) | Self::Sub(l, r) => {
                let l_ty = l.check_type(stack);
                let r_ty = r.check_type(stack);
                compare_types(l_ty, r_ty)
            }
        }
    }
}

impl TypeCheck for MulDiv {
    fn check_type(&self, stack: &ScopeStack) -> Type {
        match self {
            Self::Single(i) => i.check_type(stack),
            Self::Mul(l, r) | Self::Div(l, r) | Self::Mod(l, r) => {
                let l_ty = l.check_type(stack);
                let r_ty = r.check_type(stack);
                compare_types(l_ty, r_ty)
            }
        }
    }
}

impl TypeCheck for Node {
    fn check_type(&self, stack: &ScopeStack) -> Type {
        match self {
            Self::Single(i) => i.check_type(stack),
            Self::Plus(i) | Self::Minus(i) => i.check_type(stack)
        }
    }
}

impl TypeCheck for Core {
    fn check_type(&self, stack: &ScopeStack) -> Type {
        match self {
            Self::Str(_) => Type::Str,
            Self::Num(_) => Type::Num,
            Self::Ident(name) => stack
                .get_type_info(name)
                .map(|ti| ti.ty.clone())
                .unwrap_or(Type::NotFound),
            Self::True | Self::False => Type::Bool,
            Self::Paren(i) => i.check_type(stack),
        }
    }
}
