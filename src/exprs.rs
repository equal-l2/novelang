#[derive(Debug, Clone)]
pub struct Expr {
    pub content: items::Rel,
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
