#[derive(Clone, Debug, Default)]
pub struct Span(pub usize, pub usize);

impl From<usize> for Span {
    fn from(i: usize) -> Self {
        Self(i, i)
    }
}

pub trait Spannable {
    fn get_span(&self) -> Span;
}
