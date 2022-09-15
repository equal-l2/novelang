use crate::lex::{self, LangItem};

pub trait LookItem {
    fn item(self) -> Option<LangItem>;
}

impl LookItem for Option<&lex::Token> {
    fn item(self) -> Option<LangItem> {
        self.map(|t| t.item.clone())
    }
}
