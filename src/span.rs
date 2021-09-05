#[derive(Clone, Debug)]
pub struct Span(pub usize, pub usize);

impl From<usize> for Span {
    fn from(i: usize) -> Self {
        Self(i, i)
    }
}