use super::exprs::TypeCheck;
use super::ParseError;
pub use crate::die;
use crate::exprs::Expr;

pub(super) fn get_type(expr: &Expr, stack: &super::ScopeStack) -> super::types::Type {
    match expr.check_type(stack) {
        Ok(t) => t,
        Err(e) => die!("Invalid expr: {:?}", ParseError::from(e)), // TODO: impl Display
    }
}
