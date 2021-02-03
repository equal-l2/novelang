/// The type used to represent integer type
pub type IntType = i64;

/// The typed content of a variable
#[derive(Debug, Clone)]
pub enum Typed {
    Num(IntType),
    Bool(bool),
}
