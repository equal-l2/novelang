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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Command {
    Print,
    Sub,
    Call,
    While,
    Let,
    Modify,
    Input,
    If,
    Else,
    End,
    Roll,
    Halt,
    Break,
    Assert,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keywords {
    AsMut,
    Be,
    To,
    Dice,
    With,
    Face,
    True,
    False,
}

impl Item for Keywords {
    const DISCRIMINANTS: &'static [Self] = &[
        Self::AsMut,
        Self::Be,
        Self::To,
        Self::Dice,
        Self::With,
        Self::Face,
        Self::True,
        Self::False,
    ];

    fn as_str(&self) -> &str {
        match self {
            Self::AsMut => "asmut",
            Self::Be => "be",
            Self::To => "to",
            Self::Dice => "dice",
            Self::With => "with",
            Self::Face => "face",
            Self::True => "true",
            Self::False => "false",
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
                    if i_chars.len() == s.len() || is_sep(s[i_chars.len()]) {
                        return true;
                    }
                }
                false
            })
            .cloned()
    }
}

impl Item for Command {
    const DISCRIMINANTS: &'static [Self] = &[
        Self::Print,
        Self::Sub,
        Self::Call,
        Self::While,
        Self::Let,
        Self::Modify,
        Self::Input,
        Self::If,
        Self::Else,
        Self::End,
        Self::Roll,
        Self::Halt,
        Self::Break,
        Self::Assert,
    ];

    fn as_str(&self) -> &str {
        match self {
            Self::Print => "print",
            Self::Sub => "sub",
            Self::Call => "call",
            Self::While => "while",
            Self::Let => "let",
            Self::Modify => "modify",
            Self::Input => "input",
            Self::If => "if",
            Self::Else => "else",
            Self::End => "end",
            Self::Roll => "roll",
            Self::Halt => "halt",
            Self::Break => "break",
            Self::Assert => "assert",
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
                    if i_chars.len() == s.len() || is_sep(s[i_chars.len()]) {
                        return true;
                    }
                }
                false
            })
            .cloned()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LogOps {
    And, // &&
    Or,  // ||
}

impl Item for LogOps {
    const DISCRIMINANTS: &'static [Self] = &[
        Self::And, // &&
        Self::Or,  // ||
    ];
    fn as_str(&self) -> &str {
        match self {
            Self::And => "&&",
            Self::Or => "||",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EquOps {
    Equal,    // ==
    NotEqual, // !=
}

impl Item for EquOps {
    const DISCRIMINANTS: &'static [Self] = &[
        Self::Equal,    // ==
        Self::NotEqual, // !=
    ];
    fn as_str(&self) -> &str {
        match self {
            Self::Equal => "==",
            Self::NotEqual => "!=",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RelOps {
    LessEqual,    // <=
    GreaterEqual, // >=
    LessThan,     // <
    GreaterThan,  // >
}

impl Item for RelOps {
    const DISCRIMINANTS: &'static [Self] = &[
        Self::LessEqual,    // <=
        Self::GreaterEqual, // >=
        Self::LessThan,     // <
        Self::GreaterThan,  // >
    ];
    fn as_str(&self) -> &str {
        match self {
            Self::LessEqual => "<=",
            Self::GreaterEqual => ">=",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AddOps {
    Add, // +
    Sub, // -
}

impl Item for AddOps {
    const DISCRIMINANTS: &'static [Self] = &[
        Self::Add, // +
        Self::Sub, // -
    ];
    fn as_str(&self) -> &str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MulOps {
    Mul, // *
    Div, // /
    Mod, // %
}

impl Item for MulOps {
    const DISCRIMINANTS: &'static [Self] = &[
        Self::Mul, // *
        Self::Div, // /
        Self::Mod, // %
    ];
    fn as_str(&self) -> &str {
        match self {
            Self::Mul => "*",
            Self::Div => "/",
            Self::Mod => "%",
        }
    }
}

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
    Key(Keywords),
    Cmd(Command),
    Ops(Ops),
    Num(crate::types::IntType, usize),
    Ident(String),
    Str(String),
    Semi,
    Comma,
    LParen,
    RParen,
}

impl Items {
    pub fn len(&self) -> usize {
        use Items::*;
        match self {
            Key(i) => i.len(),
            Cmd(i) => i.len(),
            Ops(i) => i.len(),
            Num(_, l) => *l,
            Ident(i) | Str(i) => i.len(),
            Semi | Comma | LParen | RParen => 1,
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
    pub fn generate_loc_info(&self, loc: &Location) -> LocInfo {
        LocInfo {
            line: self.lines[loc.row - 1].clone(),
            loc: loc.clone(),
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
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ErrorKind::UnterminatedStr => write!(f, "String is not terminated")?,
            ErrorKind::UnexpectedChar(c) => write!(f, "Unexpected character '{}'", c)?,
        };
        let l = &self.loc_info;
        writeln!(f, " ({}:{})\n{}", l.loc.row, l.loc.col, l)?;
        Ok(())
    }
}

const RESERVED_CHARS: &[char] = &[
    '+', '-', '*', '/', '%', '"', '<', '>', '!', '=', ';', ',', '(', ')',
];

fn is_ident_char(c: char) -> bool {
    !c.is_whitespace() && !RESERVED_CHARS.contains(&c)
}

fn is_sep(c: char) -> bool {
    c.is_whitespace() || c == ';'
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
                            Items::LParen
                        }
                        ')' => {
                            i += 1;
                            Items::RParen
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
                            let confirm_item = |len| len == vs.len() || is_sep(vs[len]);
                            if is_item(&"die".chars().collect::<Vec<_>>(), vs) && confirm_item(3) {
                                // convert "die" to "dice"
                                i += 3;
                                Items::Key(Keywords::Dice)
                            } else if is_item(&"faces".chars().collect::<Vec<_>>(), vs)
                                && confirm_item(5)
                            {
                                // convert "faces" to "face"
                                i += 5;
                                Items::Key(Keywords::Face)
                            } else if let Some(res) = Keywords::parse_slice(vs) {
                                i += res.len();
                                Items::Key(res)
                            } else if let Some(res) = Command::parse_slice(vs) {
                                i += res.len();
                                Items::Cmd(res)
                            } else if let Some(res) = Ops::parse_slice(vs) {
                                i += res.len();
                                Items::Ops(res)
                            } else if v[i].is_numeric() {
                                let mut s = String::new();
                                while i < v.len() && v[i].is_numeric() {
                                    s.push(v[i]);
                                    i += 1;
                                }
                                Items::Num(s.parse().unwrap(), s.len())
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
