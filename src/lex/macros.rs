// TODO: replace this with proc macro? (to automatically generate lowercase str)
macro_rules! decl_reserved {
    ($name: ident, { $( $var:ident => $str:literal ),+ $(,)? }) => {
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub enum $name {
            $(
                $var
            ),+
        }

        impl Item for $name {
            const DISCRIMINANTS: &'static [Self] = &[
                $(
                    Self::$var
                ),+
            ];

            fn as_str(&self) -> &str {
                match self {
                    $(
                        Self::$var => $str
                    ),+
                }
            }

            fn parse_slice(s: &[char]) -> Option<Self> {
                Self::DISCRIMINANTS
                    .iter()
                    .find(|i| {
                        let i_chars: Vec<_> = i.as_str().chars().collect();
                        if is_item(&i_chars, s) {
                            // For Reserved we need this check to separate Ident
                            // (example: "be" is Reserved but "bed" is Ident)
                            if i_chars.len() == s.len() || !is_ident_char(s[i_chars.len()]) {
                                return true;
                            }
                        }
                        false
                    })
                .cloned()
            }
        }
    }
}

#[macro_export]
macro_rules! decl_ops {
    ($name: ident, { $( $var:ident => $str:literal ),+ $(,)? }) => {
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub enum $name {
            $(
                $var
            ),+
        }

        impl Item for $name {
            const DISCRIMINANTS: &'static [Self] = &[
                $(
                    Self::$var
                ),+
            ];

            fn as_str(&self) -> &str {
                match self {
                    $(
                        Self::$var => $str
                    ),+
                }
            }
        }
    }
}

pub(crate) use decl_ops;
pub(crate) use decl_reserved;
