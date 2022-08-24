// Cotains the index of the start token and the end token
#[derive(Clone, Debug, Default)]
pub struct Span(pub usize, pub usize);

impl From<usize> for Span {
    fn from(i: usize) -> Self {
        Self(i, i)
    }
}

pub trait Spannable {
    fn span(&self) -> Span;
}
