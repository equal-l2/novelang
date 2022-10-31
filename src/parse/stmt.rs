pub(self) use super::prelude;

pub mod assert;
pub mod call;
pub mod r#for;
pub mod input;
pub mod r#let;
pub mod modify;
pub mod print;
pub mod r#return;
pub mod roll;
pub mod sub;

pub mod variant {
    pub use super::assert::Assert;
    pub use super::call::Call;
    pub use super::input::Input;
    pub use super::modify::Modify;
    pub use super::print::Print;
    pub use super::r#for::For;
    pub use super::r#let::Let;
    pub use super::r#return::Return;
    pub use super::roll::Roll;
    pub use super::sub::Sub;
}
