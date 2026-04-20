pub use crate::lex::LangItem;
use crate::lex::Token;

pub trait LookItem {
    fn item(self) -> Option<LangItem>;
}

impl LookItem for Option<Token> {
    fn item(self) -> Option<LangItem> {
        self.map(|t| t.item.clone())
    }
}
