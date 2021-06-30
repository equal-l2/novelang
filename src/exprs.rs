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
    pub content: items::Rel,
}

impl Expr {
    pub fn eval_on<T: eval::VarsMap>(&self, vmap: &T) -> Result<crate::types::Typed, EvalError> {
        use eval::Eval;
        self.content.eval_on(vmap)
    }
}

pub mod items {
    #[derive(Debug, Clone)]
    pub enum Rel {
        Single(AddSub),
        Equal(AddSub, AddSub),
        NotEqual(AddSub, AddSub),
        LessEqual(AddSub, AddSub),
        GreaterEqual(AddSub, AddSub),
        LessThan(AddSub, AddSub),
        GreaterThan(AddSub, AddSub),
    }

    #[derive(Debug, Clone)]
    pub enum AddSub {
        Single(MulDiv),
        Add(MulDiv, Box<AddSub>),
        Sub(MulDiv, Box<AddSub>),
    }

    #[derive(Debug, Clone)]
    pub enum MulDiv {
        Single(Node),
        Mul(Node, Box<MulDiv>),
        Div(Node, Box<MulDiv>),
        Mod(Node, Box<MulDiv>),
    }

    #[derive(Debug, Clone)]
    pub enum Node {
        Single(Core),
        Plus(Box<Node>),
        Minus(Box<Node>),
    }

    #[derive(Debug, Clone)]
    pub enum Core {
        Str(String),
        Num(crate::types::IntType),
        Ident(String),
        True,
        False,
        Paren(Box<Rel>),
    }
}
