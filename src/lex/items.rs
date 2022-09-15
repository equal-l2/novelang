use super::{macros::*, utils::*};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LangItem {
    Key(Keyword),
    Type(TypeName),
    Bool(Boolean),
    Cmd(Command),
    Op(Ops),
    Num(crate::types::IntType, usize),
    Ident(crate::types::IdentName),
    Str(String),
    Semi,
    Comma,
    LPar,
    RPar,
    LBra,
    RBra,
}

decl_reserved!(Keyword, {
    AsMut => "asmut",
    Be => "be",
    To => "to",
    Dice => "dice",
    With => "with",
    Faces => "faces",
    From => "from",
    Results => "results",
    In => "in",
});

decl_reserved!(TypeName, {
    Bool => "bool",
    Num => "num",
    Str => "str",
});

decl_reserved!(Boolean, {
    True => "true",
    False => "false",
});

decl_reserved!(Command, {
    Print => "print",
    Sub => "sub",
    Call => "call",
    While => "while",
    Let => "let",
    Modify => "modify",
    Input => "input",
    If => "if",
    Else => "else",
    End => "end",
    Roll => "roll",
    Halt => "halt",
    Break => "break",
    Assert => "assert",
    Continue => "continue",
    For => "for",
    Return => "return",
});

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum Ops {
    Log(LogOps),
    Equ(EquOps),
    Rel(RelOps),
    Add(AddOps),
    Mul(MulOps),
}

decl_ops!(LogOps,{
    And => "&&",
    Or => "||",
});

decl_ops!(EquOps,{
    Equal => "==",
    NotEqual => "/=",
});

decl_ops!(RelOps,{
    LessEqual => "<=",
    GreaterEqual => ">=",
    LessThan => "<",
    GreaterThan => ">",
});

decl_ops!(AddOps,{
    Add => "+",
    Sub => "-",
});

decl_ops!(MulOps,{
    Mul => "*",
    Div => "/",
    Mod => "%",
});

impl LangItem {
    pub fn len(&self) -> usize {
        use LangItem::*;
        match self {
            Key(i) => i.len(),
            Type(i) => i.len(),
            Bool(i) => i.len(),
            Cmd(i) => i.len(),
            Op(i) => i.len(),
            Num(_, l) => *l,
            Ident(i) => i.as_ref().len(),
            Str(i) => i.len(),
            Semi | Comma | LPar | RPar | LBra | RBra => 1,
        }
    }
}

impl std::fmt::Display for LangItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use LangItem::*;
        match self {
            Key(i) => write!(f, "{}", i.as_str()),
            Type(i) => write!(f, "{}", i.as_str()),
            Bool(i) => write!(f, "{}", i.as_str()),
            Cmd(i) => write!(f, "{}", i.as_str()),
            Op(i) => write!(f, "{}", i.as_str()),
            Num(i, _) => write!(f, "{}", i),
            Ident(i) => write!(f, "{}", i),
            Str(i) => write!(f, "{}", i),
            Semi => write!(f, ";"),
            Comma => write!(f, ","),
            LPar => write!(f, "("),
            RPar => write!(f, ")"),
            LBra => write!(f, "["),
            RBra => write!(f, "]"),
        }
    }
}

/// Trait for token items
pub trait Item
where
    Self: Sized + Clone + 'static,
{
    const DISCRIMINANTS: &'static [Self];

    fn as_str(&self) -> &str;

    /// Parse a char slice to `Item`.
    fn parse_slice(s: &[char]) -> Option<Self> {
        Self::DISCRIMINANTS
            .iter()
            .find(|i| super::utils::is_item(&i.as_str().chars().collect::<Vec<_>>(), s))
            .cloned()
    }

    /// Return the length of token in its string form
    fn len(&self) -> usize {
        self.as_str().len()
    }
}

impl Item for Ops {
    const DISCRIMINANTS: &'static [Self] = &[];
    fn as_str(&self) -> &str {
        use Ops::*;
        match self {
            Log(i) => i.as_str(),
            Equ(i) => i.as_str(),
            Rel(i) => i.as_str(),
            Add(i) => i.as_str(),
            Mul(i) => i.as_str(),
        }
    }

    #[allow(clippy::manual_map)]
    fn parse_slice(s: &[char]) -> Option<Self> {
        if let Some(i) = LogOps::parse_slice(s) {
            Some(Self::Log(i))
        } else if let Some(i) = EquOps::parse_slice(s) {
            Some(Self::Equ(i))
        } else if let Some(i) = RelOps::parse_slice(s) {
            Some(Self::Rel(i))
        } else if let Some(i) = AddOps::parse_slice(s) {
            Some(Self::Add(i))
        } else if let Some(i) = MulOps::parse_slice(s) {
            Some(Self::Mul(i))
        } else {
            None
        }
    }
}
