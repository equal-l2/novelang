use crate::exprs::Expr;
use crate::lex;
use crate::lval::LVal;

mod exprs;

#[macro_use]
mod utils;

use utils::*;

enum ParseError {
    UnexpectedToken(lex::Token),
    EmptyExpr,
    NoPairParen { lparen: lex::Token },
    TokenExhausted,
}

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
    fn item(self) -> Option<lex::Items>;
}

impl LookItem for Option<&lex::Token> {
    fn item(self) -> Option<lex::Items> {
        self.map(|t| t.item.clone())
    }
}

fn parse_lval(i: &mut usize, lexed: &lex::Lexed) -> LVal {
    use lex::Items;
    let tks = &lexed.tokens;
    if let Some(Items::Ident(name)) = &tks.get(*i).item() {
        *i += 1;
        let mut val = LVal::Scalar(name.clone());
        loop {
            if let Some(Items::LBra) = &tks.get(*i).item() {
                *i += 1;
                let expr = parse_expr(i, lexed);
                expects!("expected RBra", Items::RBra, *i, lexed);
                val = LVal::Vector(Box::new(val), expr);
            } else {
                return val;
            }
        }
    } else {
        die_cont!("expected Ident", *i, lexed)
    }
}

pub fn parse(lexed: lex::Lexed) -> Parsed {
    use lex::{Items, Keyword};

    let mut stmts = vec![PreStmt::Ill];

    let tks = &lexed.tokens;

    let mut i = 0;
    while i < tks.len() {
        if let Items::Cmd(inst) = &tks[i].item {
            match inst {
                lex::Command::Print => parse_stmt!(i, stmts, {
                    // "Print" (<expr> {"," <expr>}) ";"
                    let mut args = vec![parse_expr(&mut i, &lexed)];

                    while i < tks.len() {
                        match &tks[i].item {
                            Items::Semi => break,
                            Items::Comma => {
                                i += 1;

                                if matches!(tks[i].item, Items::Semi) {
                                    break;
                                }

                                args.push(parse_expr(&mut i, &lexed));
                            }
                            _ => {
                                die_cont!("Unexpected token", i, lexed);
                            }
                        }
                    }
                    expects_semi!(i, lexed);
                    PreStmt::Print { args }
                }),

                lex::Command::Sub => parse_stmt!(i, stmts, {
                    // "Sub" <name> ";"

                    if let Items::Ident(name) = &tks[i].item {
                        i += 1;
                        expects_semi!(i, lexed);
                        PreStmt::Sub { name: name.clone() }
                    } else {
                        die_cont!("Expected subroutine name", i, lexed)
                    }
                }),

                lex::Command::Call => parse_stmt!(i, stmts, {
                    // "Call" <name> ";"
                    if let Items::Ident(name) = &tks[i].item {
                        i += 1;
                        expects_semi!(i, lexed);
                        PreStmt::Call { name: name.clone() }
                    } else {
                        die_cont!("Expected subroutine name", i, lexed)
                    }
                }),

                lex::Command::While => parse_stmt!(i, stmts, {
                    // "While" <cond> ";"
                    let expr = parse_expr(&mut i, &lexed);
                    expects_semi!(i, lexed);
                    PreStmt::While { cond: expr }
                }),

                lex::Command::Let => parse_stmt!(i, stmts, {
                    // "Let" <name> "Be" <expr> ("AsMut") ";"

                    if let Items::Ident(name) = &tks[i].item {
                        i += 1;
                        if name.starts_with('_') {
                            die_cont!("Identifier starts with _ is reserved", i, lexed);
                        }
                        expects!("\"Be\" expected", Items::Key(Keyword::Be), i, lexed);

                        let init = parse_expr(&mut i, &lexed);

                        expects!(
                            "\"AsMut\" or semicolon expected",
                            Items::Semi | Items::Key(Keyword::AsMut),
                            i,
                            lexed
                        );

                        let is_mut = {
                            if tks[i - 1].item == Items::Key(Keyword::AsMut) {
                                expects_semi!(i, lexed);
                                true
                            } else {
                                false
                            }
                        };

                        PreStmt::Let {
                            name: name.clone(),
                            init,
                            is_mut,
                        }
                    } else {
                        die_cont!("Ident expected", i, lexed)
                    }
                }),

                lex::Command::Modify => parse_stmt!(i, stmts, {
                    // "Modify" <lval> "To" <expr> ";"
                    let lval = parse_lval(&mut i, &lexed);
                    expects!("To expected", Items::Key(Keyword::To), i, lexed);

                    let expr = parse_expr(&mut i, &lexed);
                    expects_semi!(i, lexed);

                    PreStmt::Modify {
                        target: lval.clone(),
                        expr,
                    }
                }),

                lex::Command::If => parse_stmt!(i, stmts, {
                    // "If" <cond> ";"

                    let cond = parse_expr(&mut i, &lexed);
                    expects_semi!(i, lexed);

                    PreStmt::If { cond }
                }),

                lex::Command::Else => parse_stmt!(i, stmts, {
                    // "Else" ("If" <cond>) ";"

                    let inst_obj = if let Items::Cmd(lex::Command::If) = &tks[i].item {
                        // "Else" "If" <cond> ";"
                        i += 1;

                        let cond = parse_expr(&mut i, &lexed);
                        expects_semi!(i, lexed);

                        PreStmt::ElIf { cond }
                    } else {
                        // "Else" ";"
                        expects_semi!(i, lexed);
                        PreStmt::Else
                    };

                    inst_obj
                }),

                lex::Command::End => parse_stmt!(i, stmts, {
                    // "End" ";"
                    expects_semi!(i, lexed);
                    PreStmt::End
                }),

                lex::Command::Input => parse_stmt!(i, stmts, {
                    // "Input" (<str>) "To" <lval> ";"

                    // TODO: accept runtime prompt string
                    let prompt = if let Items::Str(prompt) = &tks[i].item {
                        i += 1;
                        Some(prompt.clone())
                    } else {
                        None
                    };

                    expects!("\"To\" expected", Items::Key(Keyword::To), i, lexed);

                    let lval = parse_lval(&mut i, &lexed);

                    expects_semi!(i, lexed);
                    PreStmt::Input {
                        prompt,
                        target: lval,
                    }
                }),

                lex::Command::Roll => parse_stmt!(i, stmts, {
                    // "Roll" <expr> "Dice" "With" <expr> "Face" "To" <lval> ";"

                    let count = parse_expr(&mut i, &lexed);

                    expects!("\"Dice\" expected", Items::Key(Keyword::Dice), i, lexed);
                    expects!("\"With\" expected", Items::Key(Keyword::With), i, lexed);

                    let face = parse_expr(&mut i, &lexed);

                    expects!("\"Face\" expected", Items::Key(Keyword::Face), i, lexed);
                    expects!("\"To\" expected", Items::Key(Keyword::To), i, lexed);

                    let lval = parse_lval(&mut i, &lexed);

                    expects_semi!(i, lexed);
                    PreStmt::Roll {
                        count,
                        face,
                        target: lval,
                    }
                }),

                lex::Command::Halt => parse_stmt!(i, stmts, {
                    // "Halt" ";"
                    expects_semi!(i, lexed);
                    PreStmt::Halt
                }),

                lex::Command::Break => parse_stmt!(i, stmts, {
                    // "Break" ";"
                    expects_semi!(i, lexed);

                    PreStmt::Break
                }),

                lex::Command::Assert => parse_stmt!(i, stmts, {
                    // "Assert" (<str> "With") <cond> ";"
                    let expr1 = parse_expr(&mut i, &lexed);

                    let mesg;
                    let cond;
                    if let Items::Key(Keyword::With) = tks[i].item {
                        // with string
                        i += 1;
                        mesg = expr1;
                        let expr2 = parse_expr(&mut i, &lexed);
                        cond = expr2;
                    } else {
                        // without string
                        mesg = Expr::from(expr1.to_string());
                        cond = expr1;
                    }
                    expects_semi!(i, lexed);

                    PreStmt::Assert { mesg, cond }
                }),

                lex::Command::Continue => parse_stmt!(i, stmts, {
                    // "Continue" ";"
                    expects_semi!(i, lexed);
                    PreStmt::Continue
                }),

                lex::Command::For => parse_stmt!(i, stmts, {
                    // "For" <name> "from" <expr> "to" <expr> ";"

                    if let Items::Ident(name) = &tks[i].item {
                        i += 1;
                        if name.starts_with('_') {
                            die_cont!("Identifier starts with _ is reserved", i, lexed);
                        }
                        expects!("\"From\" expected", Items::Key(Keyword::From), i, lexed);

                        let from = parse_expr(&mut i, &lexed);

                        expects!("\"To\" expected", Items::Key(Keyword::To), i, lexed);

                        let to = parse_expr(&mut i, &lexed);

                        expects_semi!(i, lexed);

                        PreStmt::For {
                            counter: name.clone(),
                            from,
                            to,
                        }
                    } else {
                        die_cont!("Ident expected", i, lexed)
                    }
                }),
                lex::Command::Return => parse_stmt!(i, stmts, {
                    // "Return" ";"
                    expects_semi!(i, lexed);
                    PreStmt::Return
                }),
            }
        } else {
            die_cont!("Line must begin with Command", i, lexed);
        }
    }
    Parsed { stmts }
}
