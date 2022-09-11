use crate::parse::{Ty, Type as ParseType};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Bool,
    Num,
    Str,
    Sub,
    Arr(Box<Self>),
    Nothing,
    Invalid,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.typename())
    }
}

impl Type {
    pub fn typename(&self) -> String {
        match self {
            Self::Bool => "Bool".to_owned(),
            Self::Num => "Num".to_owned(),
            Self::Str => "Str".to_owned(),
            Self::Sub => "Sub".to_owned(),
            Self::Arr(i) => format!("Arr[{}]", i.typename()),
            Self::Nothing => "Nothing".to_owned(),
            Self::Invalid => "Invalid".to_owned(),
        }
    }

    pub fn is_printable(&self) -> bool {
        match self {
            Self::Bool | Self::Num | Self::Str => true,
            Self::Arr(i) => i.is_printable(),
            _ => false,
        }
    }
}

impl From<ParseType> for Type {
    fn from(pt: ParseType) -> Self {
        match pt.ty() {
            Ty::Nothing => Self::Nothing,
            Ty::Num => Self::Num,
            Ty::Bool => Self::Bool,
            Ty::Str => Self::Str,
        }
    }
}
