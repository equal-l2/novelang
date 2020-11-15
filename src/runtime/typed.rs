use crate::types::IntType;

/// Represents the typed content of a variable
#[derive(Debug, Clone)]
pub enum Typed {
    Num(IntType),
    Bool(bool),
}
