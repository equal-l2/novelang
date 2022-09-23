use crate::parse::types::{Ty, Type as ParseType};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Bool,
    Num,
    Str,
    Sub {
        args: Option<Vec<Self>>,
        res: Option<Box<Self>>,
    },
    Arr(Box<Self>),
    Invalid,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool => write!(f, "Bool"),
            Self::Num => write!(f, "Num"),
            Self::Str => write!(f, "Str"),
            Self::Sub { args, res } => {
                if let Some(args) = args {
                    let mut it = args.iter();
                    let first = it.next().unwrap();
                    write!(f, "{first}")?;
                    for arg in it {
                        write!(f, ", {arg}")?;
                    }
                }
                write!(f, ")")?;

                if let Some(res) = res {
                    write!(f, " -> {res}")?;
                }

                Ok(())
            }
            Self::Arr(i) => write!(f, "Arr[{}]", i.typename()),
            Self::Invalid => write!(f, "Invalid"),
        }
    }
}

impl Type {
    pub fn typename(&self) -> String {
        self.to_string()
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
            Ty::Num => Self::Num,
            Ty::Bool => Self::Bool,
            Ty::Str => Self::Str,
        }
    }
}
