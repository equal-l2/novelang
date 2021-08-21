#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Bool,
    Num,
    Str,
    Sub,
    Arr(Box<Type>),
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
        }
    }
}
