pub trait Item
where
    Self: Sized + Clone + 'static,
{
    const DISCRIMINANTS: &'static [Self];

    fn as_str(&self) -> &str;

    fn check(s: &[char]) -> Option<Self> {
        Self::DISCRIMINANTS
            .iter()
            .find(|i| is_item(&i.as_str().chars().collect::<Vec<_>>(), s))
            .cloned()
    }

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
pub enum Insts {
    Print,
    Sub,
    Call,
    While,
    Let,
    Modify,
    Input,
    If,
    ElIf,
    Else,
    End,
    Roll,
    Halt,
    Break,
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

    fn check(s: &[char]) -> Option<Self> {
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

impl Item for Insts {
    const DISCRIMINANTS: &'static [Self] = &[
        Self::Print,
        Self::Sub,
        Self::Call,
        Self::While,
        Self::Let,
        Self::Modify,
        Self::Input,
        Self::If,
        Self::ElIf,
        Self::Else,
        Self::End,
        Self::Roll,
        Self::Halt,
        Self::Break,
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
            Self::ElIf => "elif",
            Self::Else => "else",
            Self::End => "end",
            Self::Roll => "roll",
            Self::Halt => "halt",
            Self::Break => "break",
        }
    }

    fn check(s: &[char]) -> Option<Self> {
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
pub enum AriOps {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %
}

impl Item for AriOps {
    const DISCRIMINANTS: &'static [Self] = &[
        Self::Add, // +
        Self::Sub, // -
        Self::Mul, // *
        Self::Div, // /
        Self::Mod, // %
    ];
    fn as_str(&self) -> &str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Mod => "%",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RelOps {
    Equal,        // ==
    NotEqual,     // !=
    LessEqual,    // <=
    GreaterEqual, // >=
    LessThan,     // <
    GreaterThan,  // >
}

impl Item for RelOps {
    const DISCRIMINANTS: &'static [Self] = &[
        Self::Equal,        // ==
        Self::NotEqual,     // !=
        Self::LessEqual,    // <=
        Self::GreaterEqual, // >=
        Self::LessThan,     // <
        Self::GreaterThan,  // >
    ];
    fn as_str(&self) -> &str {
        match self {
            Self::Equal => "==",
            Self::NotEqual => "!=",
            Self::LessEqual => "<=",
            Self::GreaterEqual => ">=",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ops {
    Ari(AriOps),
    Rel(RelOps),
}

impl Item for Ops {
    const DISCRIMINANTS: &'static [Self] = &[];
    fn as_str(&self) -> &str {
        match self {
            Self::Ari(i) => i.as_str(),
            Self::Rel(i) => i.as_str(),
        }
    }

    fn check(s: &[char]) -> Option<Self> {
        if let Some(i) = AriOps::check(s) {
            Some(Self::Ari(i))
        } else if let Some(i) = RelOps::check(s) {
            Some(Self::Rel(i))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Items {
    Key(Keywords),
    Inst(Insts),
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
            Inst(i) => i.len(),
            Ops(i) => i.len(),
            Num(_, l) => *l,
            Ident(i) => i.len(),
            Str(i) => i.len(), //NOTE: is this right? (+2 may be required)
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

const RESERVED_CHARS: &[char] = &['+', '-', '*', '/', '%', '"', '<', '>', '!', '=', ';', ','];

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
                            if is_item(&"dices".chars().collect::<Vec<_>>(), vs) && confirm_item(5)
                            {
                                // convert "dices" to "dice"
                                i += 5;
                                Items::Key(Keywords::Dice)
                            } else if is_item(&"faces".chars().collect::<Vec<_>>(), vs)
                                && confirm_item(5)
                            {
                                // convert "faces" to "face"
                                i += 5;
                                Items::Key(Keywords::Face)
                            } else if let Some(res) = Keywords::check(vs) {
                                i += res.len();
                                Items::Key(res)
                            } else if let Some(res) = Insts::check(vs) {
                                i += res.len();
                                Items::Inst(res)
                            } else if let Some(res) = Ops::check(vs) {
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
