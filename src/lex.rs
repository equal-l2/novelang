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
            .find(|i| is_item(&i.as_str().chars().collect::<Vec<_>>(), s))
            .cloned()
    }

    /// Return the length of token in its string form
    fn len(&self) -> usize {
        self.as_str().len()
    }
}

fn is_item(item_chars: &[char], src_chars: &[char]) -> bool {
    item_chars.len() <= src_chars.len()
        && item_chars
            .iter()
            .zip(src_chars)
            .all(|(i, s)| i.to_lowercase().eq(s.to_lowercase()))
}

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

decl_reserved!(Keyword, {
    AsMut => "asmut",
    Be => "be",
    To => "to",
    Dice => "dice",
    With => "with",
    Face => "face",
    True => "true",
    False => "false",
    From => "from",
});

decl_ops!(LogOps,{
    And => "&&",
    Or => "||",
});

decl_ops!(EquOps,{
    Equal => "==",
    NotEqual => "!=",
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ops {
    Log(LogOps),
    Equ(EquOps),
    Rel(RelOps),
    Add(AddOps),
    Mul(MulOps),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Items {
    Key(Keyword),
    Cmd(Command),
    Op(Ops),
    Num(crate::types::IntType, usize),
    Ident(String),
    Str(String),
    Semi,
    Comma,
    LPar,
    RPar,
    LBra,
    RBra,
}

impl Items {
    pub fn len(&self) -> usize {
        use Items::*;
        match self {
            Key(i) => i.len(),
            Cmd(i) => i.len(),
            Op(i) => i.len(),
            Num(_, l) => *l,
            Ident(i) | Str(i) => i.len(),
            Semi | Comma | LPar | RPar | LBra | RBra => 1,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Location {
    pub row: usize,
    pub col: usize,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub loc: Location,
    pub item: Items,
}

impl Token {
    pub fn next_col_loc(&self) -> Location {
        let loc = &self.loc;
        Location {
            row: loc.row,
            col: loc.col + self.item.len(),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{ {:?} ({},{}) }}",
            self.item, self.loc.row, self.loc.col
        )
    }
}

#[derive(Debug, Clone)]
pub struct Lexed {
    pub lines: Vec<String>,
    pub tokens: Vec<Token>,
}

#[derive(Debug, Clone)]
pub struct LocInfo {
    line: String,
    loc: Location,
}

impl std::fmt::Display for LocInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let row = self.loc.row;
        let col = self.loc.col;
        writeln!(f, "     |")?;
        writeln!(f, "{:<4} | {}", row, self.line)?;
        writeln!(f, "     | {:>1$}", "^", col)?;
        writeln!(f, "     |")?;
        Ok(())
    }
}

impl Lexed {
    pub fn generate_error_mesg(&self, idx: usize) -> LocInfo {
        let loc = self.tokens.get(idx).map_or_else(
            || self.tokens.last().unwrap().next_col_loc(),
            |tk| tk.loc.clone(),
        );

        LocInfo {
            line: self.lines[loc.row - 1].clone(),
            loc,
        }
    }
}

impl std::fmt::Display for Lexed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut i = 0;
        for idx in 0..self.lines.len() {
            writeln!(f, "{:4>} |{}", idx + 1, self.lines[idx])?;
            while i < self.tokens.len() && self.tokens[i].loc.row == idx + 1 {
                write!(f, "{:?} ", self.tokens[i].item)?;
                i += 1;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    loc_info: LocInfo,
    kind: ErrorKind,
}

impl std::error::Error for Error {}

#[derive(Debug, Clone)]
enum ErrorKind {
    UnterminatedStr,
    UnexpectedChar(char),
    TooLongNum,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ErrorKind::UnterminatedStr => write!(f, "String is not terminated")?,
            ErrorKind::UnexpectedChar(c) => write!(f, "Unexpected character '{}'", c)?,
            ErrorKind::TooLongNum => write!(
                f,
                "Number is too long for {}-bit integer",
                std::mem::size_of::<crate::types::IntType>() * 8
            )?,
        };
        let l = &self.loc_info;
        writeln!(f, " ({}:{})\n{}", l.loc.row, l.loc.col, l)?;
        Ok(())
    }
}

const RESERVED_CHARS: &[char] = &[
    '+', '-', '*', '/', '%', '"', '<', '>', '!', '=', ';', ',', '(', ')', '[', ']',
];

fn is_ident_char(c: char) -> bool {
    !c.is_whitespace() && !RESERVED_CHARS.contains(&c)
}

pub fn lex(s: String) -> Result<Lexed, Error> {
    let mut tks = Vec::new();
    let lines: Vec<_> = s.lines().map(String::from).collect();
    for (idx, l) in lines.iter().enumerate() {
        let v: Vec<_> = l.chars().collect();
        let mut i = 0;
        while i < v.len() {
            if v[i].is_whitespace() {
                i += 1;
            } else {
                let loc = Location {
                    row: idx + 1,
                    col: i + 1,
                };
                tks.push(Token {
                    loc: loc.clone(),
                    item: match v[i] {
                        '#' => {
                            break;
                        }
                        ';' => {
                            i += 1;
                            Items::Semi
                        }
                        ',' => {
                            i += 1;
                            Items::Comma
                        }
                        '(' => {
                            i += 1;
                            Items::LPar
                        }
                        ')' => {
                            i += 1;
                            Items::RPar
                        }
                        '[' => {
                            i += 1;
                            Items::LBra
                        }
                        ']' => {
                            i += 1;
                            Items::RBra
                        }
                        '"' => {
                            i += 1;
                            let mut s = String::new();
                            loop {
                                if i >= v.len() {
                                    return Err(Error {
                                        loc_info: LocInfo {
                                            line: l.clone(),
                                            loc,
                                        },
                                        kind: ErrorKind::UnterminatedStr,
                                    });
                                }
                                if v[i] == '"' {
                                    i += 1;
                                    break Items::Str(s);
                                }
                                s.push(v[i]);
                                i += 1;
                            }
                        }
                        _ => {
                            let vs = &v[i..];
                            let confirm_item = |len| len == vs.len() || !is_ident_char(vs[len]);
                            if is_item(&"die".chars().collect::<Vec<_>>(), vs) && confirm_item(3) {
                                // convert "die" to "dice"
                                i += 3;
                                Items::Key(Keyword::Dice)
                            } else if is_item(&"faces".chars().collect::<Vec<_>>(), vs)
                                && confirm_item(5)
                            {
                                // convert "faces" to "face"
                                i += 5;
                                Items::Key(Keyword::Face)
                            } else if let Some(res) = Keyword::parse_slice(vs) {
                                i += res.len();
                                Items::Key(res)
                            } else if let Some(res) = Command::parse_slice(vs) {
                                i += res.len();
                                Items::Cmd(res)
                            } else if let Some(res) = Ops::parse_slice(vs) {
                                i += res.len();
                                Items::Op(res)
                            } else if v[i].is_numeric() {
                                let mut s = String::new();
                                while i < v.len() && v[i].is_numeric() {
                                    s.push(v[i]);
                                    i += 1;
                                }
                                match s.parse() {
                                    Ok(i) => Items::Num(i, s.len()),
                                    Err(_) => {
                                        return Err(Error {
                                            loc_info: LocInfo {
                                                line: l.clone(),
                                                loc,
                                            },
                                            kind: ErrorKind::TooLongNum,
                                        })
                                    }
                                }
                            } else if is_ident_char(v[i]) {
                                let mut s = String::new();
                                while i < v.len() && is_ident_char(v[i]) {
                                    s.push(v[i]);
                                    i += 1;
                                }
                                Items::Ident(s)
                            } else {
                                eprintln!("{:?}", tks);
                                return Err(Error {
                                    loc_info: LocInfo {
                                        line: l.clone(),
                                        loc,
                                    },
                                    kind: ErrorKind::UnexpectedChar(v[i]),
                                });
                            }
                        }
                    },
                });
            }
        }
    }

    Ok(Lexed { lines, tokens: tks })
}
