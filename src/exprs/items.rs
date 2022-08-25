use super::Span;
pub type TopItem = Log;
pub type TopNum = AddSub;

#[derive(Clone, Debug)]
pub enum Log {
    Single(Equ),
    And(Box<Self>, Equ),
    Or(Box<Self>, Equ),
}

#[derive(Clone, Debug)]
pub enum Equ {
    Single(Rel),
    Equal(Box<Self>, Rel),
    NotEqual(Box<Self>, Rel),
}

#[derive(Clone, Debug)]
pub enum Rel {
    Single(AddSub),
    LessEqual(AddSub, AddSub),
    GreaterEqual(AddSub, AddSub),
    LessThan(AddSub, AddSub),
    GreaterThan(AddSub, AddSub),
}

#[derive(Clone, Debug)]
pub enum AddSub {
    Single(MulDiv),
    Add(Box<Self>, MulDiv),
    Sub(Box<Self>, MulDiv),
}

#[derive(Clone, Debug)]
pub enum MulDiv {
    Single(Node),
    Mul(Box<Self>, Node),
    Div(Box<Self>, Node),
    Mod(Box<Self>, Node),
}

#[derive(Clone, Debug)]
pub enum Node {
    Single(Value),
    Plus(Box<Self>, Span),
    Minus(Box<Self>, Span),
}

#[derive(Clone, Debug)]
pub enum Value {
    Single(Core),
    ArrElem(Box<Self>, Box<TopNum>, Span),
}

#[derive(Clone, Debug)]
pub enum Core {
    Str(String, Span),
    Num(crate::types::IntType, Span),
    Ident(crate::target::Ident),
    True(Span),
    False(Span),
    Paren(Box<TopItem>, Span),
    Arr(Vec<TopItem>, Span),
}
