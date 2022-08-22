use crate::exprs::Expr;
use crate::lex;
use crate::lval::LVal;
use crate::span::{Span, Spannable};

mod exprs;

#[macro_use]
mod utils;

use utils::*;

#[derive(Debug)]
pub struct Error(pub String, pub Span);

#[derive(Debug, Clone)]
pub enum PreStmt {
    Print {
        args: Vec<Expr>,
    },
    Sub {
        name: String,
    },
    Call {
        name: String,
    },
    While {
        cond: Expr,
    },
    Let {
        name: String,
        init: Expr,
        is_mut: bool,
    },
    Modify {
        target: LVal,
        expr: Expr,
    },
    If {
        cond: Expr,
    },
    ElIf {
        cond: Expr,
    },
    Else,
    End,
    Input {
        prompt: Option<String>,
        target: LVal,
    },
    Roll {
        count: Expr,
        face: Expr,
        target: LVal,
    },
    Halt,
    Ill,
    Break,
    Assert {
        mesg: Expr,
        cond: Expr,
    },
    Continue,
    For {
        counter: String,
        from: Expr,
        to: Expr,
    },
    Return,
}

#[derive(Debug, Clone)]
pub struct Parsed {
    pub stmts: Vec<PreStmt>,
}

trait LookItem {
    fn item(self) -> Option<lex::LangItem>;
}

impl LookItem for Option<&lex::Token> {
    fn item(self) -> Option<lex::LangItem> {
        self.map(|t| t.item.clone())
    }
}

fn parse_lval<'a, T>(tks: &mut std::iter::Peekable<T>, last: usize) -> Result<LVal, Error>
where
    T: Iterator<Item = (usize, &'a lex::Token)>,
{
    use lex::LangItem;

    let (i, tk) = tks
        .peek()
        .ok_or_else(|| Error("expected Ident".into(), last.into()))?;
    if let LangItem::Ident(name) = &tk.item {
        let mut val = LVal::Scalar(name.clone());
        let _ = tks.next().unwrap();
        loop {
            if tks.peek().item() == Some(LangItem::LBra) {
                let _ = tks.next().unwrap();
                let expr = parse_expr(tks, last)?;
                expects!("expected RBra", LangItem::RBra, tks, last);
                val = LVal::Vector(Box::from(val), Box::from(expr));
            } else {
                return Ok(val);
            }
        }
    } else {
        Err(Error("expected Ident".into(), (*i).into()))
    }
}

pub fn parse(lexed: &lex::Lexed) -> Result<Parsed, Error> {
    use lex::{LangItem, Keyword};

    let mut stmts = vec![PreStmt::Ill];

    let last = lexed.tokens.len();
    let mut tks = lexed.tokens.iter().enumerate().peekable();

    while let Some((idx, tok)) = tks.next() {
        if let LangItem::Cmd(inst) = &tok.item {
            match inst {
                lex::Command::Print => parse_stmt!(stmts, {
                    // "Print" (<expr> {"," <expr>}) ";"
                    let mut args = vec![parse_expr(&mut tks, last)?];

                    while let Some((i, tk)) = tks.peek() {
                        match tk.item {
                            LangItem::Semi => break,
                            LangItem::Comma => {
                                let _ = tks.next().unwrap();

                                if matches!(tks.peek().item(), Some(LangItem::Semi)) {
                                    break;
                                }

                                args.push(parse_expr(&mut tks, last)?);
                            }
                            _ => {
                                return Err(Error("Unexpected token".into(), (*i).into()));
                            }
                        }
                    }
                    expects_semi!(tks, last);
                    PreStmt::Print { args }
                }),

                lex::Command::Sub => parse_stmt!(stmts, {
                    // "Sub" <name> ";"
                    let (i, tk) = tks
                        .next()
                        .ok_or_else(|| Error("expected subrountine name".into(), last.into()))?;
                    if let LangItem::Ident(name) = &tk.item {
                        expects_semi!(tks, last);
                        PreStmt::Sub { name: name.clone() }
                    } else {
                        return Err(Error("Expected subroutine name".into(), i.into()));
                    }
                }),

                lex::Command::Call => parse_stmt!(stmts, {
                    // "Call" <name> ";"
                    let (i, tk) = tks
                        .next()
                        .ok_or_else(|| Error("expected subrountine name".into(), last.into()))?;
                    if let LangItem::Ident(name) = &tk.item {
                        expects_semi!(tks, last);
                        PreStmt::Call { name: name.clone() }
                    } else {
                        return Err(Error("Expected subroutine name".into(), i.into()));
                    }
                }),

                lex::Command::While => parse_stmt!(stmts, {
                    // "While" <cond> ";"
                    let expr = parse_expr(&mut tks, last)?;
                    expects_semi!(tks, last);
                    PreStmt::While { cond: expr }
                }),

                lex::Command::Let => parse_stmt!(stmts, {
                    // "Let" <name> "Be" <expr> ("AsMut") ";"

                    let (i, tk) = tks
                        .next()
                        .ok_or_else(|| Error("expected Ident".into(), last.into()))?;
                    if let LangItem::Ident(name) = &tk.item {
                        if name.starts_with('_') {
                            return Err(Error(
                                "Identifier starts with _ is reserved".into(),
                                i.into(),
                            ));
                        }
                        expects!("\"Be\" expected", LangItem::Key(Keyword::Be), tks, last);

                        let init = parse_expr(&mut tks, last)?;

                        let i = init.span().1;

                        match tks.next() {
                            Some((_, tk)) => match tk.item {
                                LangItem::Key(Keyword::AsMut) => {
                                    expects_semi!(tks, last);
                                    PreStmt::Let {
                                        name: name.clone(),
                                        init,
                                        is_mut: true,
                                    }
                                }
                                LangItem::Semi => PreStmt::Let {
                                    name: name.clone(),
                                    init,
                                    is_mut: false,
                                },
                                _ => {
                                    return Err(Error(
                                        "expected Semicolon or AsMut".into(),
                                        (i + 1).into(),
                                    ))
                                }
                            },
                            None => {
                                return Err(Error(
                                    "expected Semicolon or AsMut".into(),
                                    (i + 1).into(),
                                ));
                            }
                        }
                    } else {
                        return Err(Error("Ident expected".into(), i.into()));
                    }
                }),

                lex::Command::Modify => parse_stmt!(stmts, {
                    // "Modify" <lval> "To" <expr> ";"
                    let lval = parse_lval(&mut tks, last)?;
                    expects!("To expected", LangItem::Key(Keyword::To), tks, last);

                    let expr = parse_expr(&mut tks, last)?;
                    expects_semi!(tks, last);

                    PreStmt::Modify {
                        target: lval.clone(),
                        expr,
                    }
                }),

                lex::Command::If => parse_stmt!(stmts, {
                    // "If" <cond> ";"

                    let cond = parse_expr(&mut tks, last)?;
                    expects_semi!(tks, last);

                    PreStmt::If { cond }
                }),

                lex::Command::Else => parse_stmt!(stmts, {
                    // "Else" ("If" <cond>) ";"

                    let (_, tk) = tks
                        .peek()
                        .ok_or_else(|| Error("expected Semicolon or If".into(), last.into()))?;

                    if tk.item == LangItem::Cmd(lex::Command::If) {
                        // "Else" "If" <cond> ";"
                        let _ = tks.next().unwrap();

                        let cond = parse_expr(&mut tks, last)?;
                        expects_semi!(tks, last);

                        PreStmt::ElIf { cond }
                    } else {
                        // "Else" ";"
                        expects_semi!(tks, last);
                        PreStmt::Else
                    }
                }),

                lex::Command::End => parse_stmt!(stmts, {
                    // "End" ";"
                    expects_semi!(tks, last);
                    PreStmt::End
                }),

                lex::Command::Input => parse_stmt!(stmts, {
                    // "Input" (<str>) "To" <lval> ";"

                    // TODO: accept runtime prompt string
                    let (_, tk) = tks
                        .peek()
                        .ok_or_else(|| Error("expected To".into(), last.into()))?;

                    let prompt = if let LangItem::Str(prompt) = &tk.item {
                        let _ = tks.next().unwrap();
                        Some(prompt.clone())
                    } else {
                        None
                    };

                    expects!("\"To\" expected", LangItem::Key(Keyword::To), tks, last);

                    let lval = parse_lval(&mut tks, last)?;

                    expects_semi!(tks, last);
                    PreStmt::Input {
                        prompt,
                        target: lval,
                    }
                }),

                lex::Command::Roll => parse_stmt!(stmts, {
                    // "Roll" <expr> "Dice" "With" <expr> "Faces" "To" <lval> ";"

                    let count = parse_expr(&mut tks, last)?;

                    expects!("\"Dice\" expected", LangItem::Key(Keyword::Dice), tks, last);
                    expects!("\"With\" expected", LangItem::Key(Keyword::With), tks, last);

                    let face = parse_expr(&mut tks, last)?;

                    expects!("\"Faces\" expected", LangItem::Key(Keyword::Faces), tks, last);
                    expects!("\"To\" expected", LangItem::Key(Keyword::To), tks, last);

                    let lval = parse_lval(&mut tks, last)?;

                    expects_semi!(tks, last);
                    PreStmt::Roll {
                        count,
                        face,
                        target: lval,
                    }
                }),

                lex::Command::Halt => parse_stmt!(stmts, {
                    // "Halt" ";"
                    expects_semi!(tks, last);
                    PreStmt::Halt
                }),

                lex::Command::Break => parse_stmt!(stmts, {
                    // "Break" ";"
                    expects_semi!(tks, last);

                    PreStmt::Break
                }),

                lex::Command::Assert => parse_stmt!(stmts, {
                    // "Assert" (<str> "With") <cond> ";"
                    let expr1 = parse_expr(&mut tks, last)?;

                    let (_, tk) = tks
                        .peek()
                        .ok_or_else(|| Error("expected With or Semicolon".into(), last.into()))?;

                    let mesg;
                    let cond;
                    if tk.item == LangItem::Key(Keyword::With) {
                        // with string
                        let _ = tks.next().unwrap();
                        mesg = expr1;
                        let expr2 = parse_expr(&mut tks, last)?;
                        cond = expr2;
                    } else {
                        // without string
                        mesg = Expr::from(expr1.to_string());
                        cond = expr1;
                    }
                    expects_semi!(tks, last);

                    PreStmt::Assert { mesg, cond }
                }),

                lex::Command::Continue => parse_stmt!(stmts, {
                    // "Continue" ";"
                    expects_semi!(tks, last);
                    PreStmt::Continue
                }),

                lex::Command::For => parse_stmt!(stmts, {
                    // "For" <name> "from" <expr> "to" <expr> ";"

                    let (i, tk) = tks
                        .next()
                        .ok_or_else(|| Error("expected Ident".into(), last.into()))?;

                    if let LangItem::Ident(name) = &tk.item {
                        if name.starts_with('_') {
                            return Err(Error(
                                "Identifier starts with _ is reserved".into(),
                                i.into(),
                            ));
                        }
                        expects!("\"From\" expected", LangItem::Key(Keyword::From), tks, last);

                        let from = parse_expr(&mut tks, last)?;

                        expects!("\"To\" expected", LangItem::Key(Keyword::To), tks, last);

                        let to = parse_expr(&mut tks, last)?;

                        expects_semi!(tks, last);

                        PreStmt::For {
                            counter: name.clone(),
                            from,
                            to,
                        }
                    } else {
                        return Err(Error("Ident expected".into(), i.into()));
                    }
                }),
                lex::Command::Return => parse_stmt!(stmts, {
                    // "Return" ";"
                    expects_semi!(tks, last);
                    PreStmt::Return
                }),
            }
        } else {
            return Err(Error("Line must begin with Command".into(), idx.into()));
        }
    }
    Ok(Parsed { stmts })
}
