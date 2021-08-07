mod display;
mod eval;

pub use eval::VarsMap;

#[derive(Debug)]
pub enum EvalError {
    VariableNotFound(String),
    OverFlow,
    ZeroDivision,
    TypeError(String),
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Failed to eval because ")?;
        match self {
            Self::VariableNotFound(s) => write!(f, "variable {} was not found", s),
            Self::OverFlow => write!(f, "of overflow"),
            Self::ZeroDivision => write!(f, "of zero division"),
            Self::TypeError(s) => write!(f, "of type error: {}", s),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub content: items::Equ,
}

impl Expr {
    pub fn eval_on<T: eval::VarsMap>(&self, vmap: &T) -> Result<crate::types::Typed, EvalError> {
        use eval::Eval;
        self.content.eval_on(vmap)
    }

    pub fn new_str(s: String) -> Self {
        use items::*;
        Self {
            content: Equ::Single(Rel::Single(AddSub::Single(MulDiv::Single(Node::Single(
                Core::Str(s),
            ))))),
        }
    }
}

pub mod items {
    #[derive(Debug, Clone)]
    pub enum Equ {
        Single(Rel),
        Equal(Box<Self>, Rel),
        NotEqual(Box<Self>, Rel),
    }

    #[derive(Debug, Clone)]
    pub enum Rel {
        Single(AddSub),
        LessEqual(AddSub, AddSub),
        GreaterEqual(AddSub, AddSub),
        LessThan(AddSub, AddSub),
        GreaterThan(AddSub, AddSub),
    }

    #[derive(Debug, Clone)]
    pub enum AddSub {
        Single(MulDiv),
        Add(Box<Self>, MulDiv),
        Sub(Box<Self>, MulDiv),
    }

    #[derive(Debug, Clone)]
    pub enum MulDiv {
        Single(Node),
        Mul(Box<Self>, Node),
        Div(Box<Self>, Node),
        Mod(Box<Self>, Node),
    }

    #[derive(Debug, Clone)]
    pub enum Node {
        Single(Core),
        Plus(Box<Self>),
        Minus(Box<Self>),
    }

    #[derive(Debug, Clone)]
    pub enum Core {
        Str(String),
        Num(crate::types::IntType),
        Ident(String),
        True,
        False,
        Paren(Box<Equ>),
    }
}
