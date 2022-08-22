use derive_more::*;
#[derive(Debug, Clone, PartialEq, Eq, From, Display, Hash, AsRef)]
pub struct IdentName(String);

impl From<&str> for IdentName {
    fn from(s: &str) -> Self {
        s.to_string().into()
    }
}

/// The type used to represent integer type
pub type IntType = i64;
