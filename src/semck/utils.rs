use super::{exprs::TypeCheck, Expr, ParseError};
pub use crate::die;

pub(super) fn get_type(expr: &Expr, stack: &super::ScopeStack) -> super::types::Type {
    match expr.check_type(stack) {
        Ok(t) => t,
        Err(e) => die!("Invalid expr: {:?}", ParseError::from(e)), // TODO: impl Display
    }
}
