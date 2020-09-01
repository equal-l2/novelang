trait ToItem
where
    Self: Sized + Copy + 'static,
{
    const DISCRIMINANTS: &'static [Self];

    fn as_str(&self) -> &str;

    fn check(s: &[char]) -> Option<Self> {
        'outer: for res in Self::DISCRIMINANTS {
            let res_chars: Vec<_> = res.as_str().chars().collect();
            if res_chars.len() <= s.len() {
                for i in 0..res_chars.len() {
                    if res_chars[i].to_lowercase().ne(s[i].to_lowercase()) {
                        continue 'outer;
                    }
                }
                return Some(*res);
            }
        }
        None
    }

    fn len(&self) -> usize {
        self.as_str().len()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Reserved {
    // keywords
    AsMut,
    Be,
    To,
    Dice,
    With,
    Face,
    // insts
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
    EnableWait,
    DisableWait,
}

impl ToItem for Reserved {
    const DISCRIMINANTS: &'static [Self] = &[
        // keywords
        Self::AsMut,
        Self::Be,
        Self::To,
        Self::Dice,
        Self::With,
        Self::Face,
        // insts
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
        Self::EnableWait,
        Self::DisableWait,
    ];

    fn as_str(&self) -> &str {
        match self {
            // keywords
            Self::AsMut => "asmut",
            Self::Be => "be",
            Self::To => "to",
            Self::Dice => "dice",
            Self::With => "with",
            Self::Face => "face",
            // insts
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
            Self::EnableWait => "enablewait",
            Self::DisableWait => "disablewait",
        }
    }

    fn check(s: &[char]) -> Option<Self> {
        'outer: for res in Self::DISCRIMINANTS {
            let res_chars: Vec<_> = res.as_str().chars().collect();
            if res_chars.len() <= s.len() {
                for i in 0..res_chars.len() {
                    if res_chars[i].to_lowercase().ne(s[i].to_lowercase()) {
                        continue 'outer;
                    }
                }
                // For Reserved we need this check to separate Ident
                // (example: "be" is Reserved but "bed" is Ident)
                if res_chars.len() == s.len() || is_sep(s[res_chars.len()]) {
                    return Some(*res);
                }
            }
        }
        None
    }
}

#[derive(Debug, Clone, Copy)]
enum AriOps {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %
}

impl ToItem for AriOps {
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

#[derive(Debug, Clone, Copy)]
enum RelOps {
    Equal,        // ==
    NotEqual,     // !=
    LessEqual,    // <=
    GreaterEqual, // >=
    LessThan,     // <
    GreaterThan,  // >
}

impl ToItem for RelOps {
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

type NumType = i64;
#[derive(Debug, Clone)]
enum Item {
    Res(Reserved),
    Ari(AriOps),
    Rel(RelOps),
    Num(NumType),
    Ident(String),
    Str(String),
    Semi,
    Comma,
}

#[derive(Debug, Clone)]
pub struct Location {
    pub row: usize,
    pub col: usize,
}

#[derive(Clone)]
pub struct Token {
    pub loc: Location,
    item: Item,
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "Token {{ {:?} ({},{}) }}",
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
pub enum Error {
    UnterminatedStr,
}

const RESERVED_CHARS: &'static [char] =
    &['+', '-', '*', '/', '%', '"', '<', '>', '!', '=', ';', ','];

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
        eprintln!("line {}", idx);
        let v: Vec<_> = l.chars().collect();
        let mut i = 0;
        while i < v.len() {
            let loc = Location {
                row: idx + 1,
                col: i + 1,
            };
            if v[i] == '#' {
                break;
            }
            if v[i].is_whitespace() {
                i += 1;
            } else if v[i] == ';' {
                tks.push(Token {
                    loc,
                    item: Item::Semi,
                });
                i += 1;
            } else if v[i] == ',' {
                tks.push(Token {
                    loc,
                    item: Item::Comma,
                });
                i += 1;
            } else if v[i] == '"' {
                i += 1;
                let mut s = String::new();
                while i < v.len() {
                    if v[i] == '"' {
                        break;
                    }
                    s.push(v[i]);
                    i += 1;
                }
                if v[i] != '"' {
                    return Err(Error::UnterminatedStr);
                }
                tks.push(Token {
                    loc,
                    item: Item::Str(s),
                });
                i += 1;
            } else if let Some(res) = Reserved::check(&v[i..v.len()]) {
                tks.push(Token {
                    loc,
                    item: Item::Res(res),
                });
                i += res.len();
            } else if let Some(res) = AriOps::check(&v[i..v.len()]) {
                tks.push(Token {
                    loc,
                    item: Item::Ari(res),
                });
                i += res.len();
            } else if let Some(res) = RelOps::check(&v[i..v.len()]) {
                tks.push(Token {
                    loc,
                    item: Item::Rel(res),
                });
                i += res.len();
            } else if v[i].is_numeric() {
                let mut s = String::new();
                while i < v.len() && v[i].is_numeric() {
                    s.push(v[i]);
                    i += 1;
                }
                tks.push(Token {
                    loc,
                    item: Item::Num(s.parse().unwrap()),
                });
            } else {
                let mut s = String::new();
                while i < v.len() && is_ident_char(v[i]) {
                    s.push(v[i]);
                    i += 1;
                }
                tks.push(Token {
                    loc,
                    item: Item::Ident(s),
                });
            }
        }
    }
    Ok(Lexed { lines, tokens: tks })
}
