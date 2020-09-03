use crate::exprs::{self, Expr};
use crate::lex;

#[derive(Debug, Clone)]
pub enum PrintArgs {
    Str(String),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Insts {
    Print {
        args: Vec<PrintArgs>,
    },
    Sub {
        name: String,
        offset_to_end: usize,
    },
    Call {
        name: String,
    },
    While {
        cond: Expr,
        offset_to_end: usize,
    },
    Let {
        name: String,
        init: Expr,
        is_mut: bool,
    },
    Modify {
        name: String,
        expr: Expr,
    },
    If {
        cond: Expr,
        offset_to_next: usize,
    },
    ElIf {
        cond: Expr,
        offset_to_next: usize,
    },
    Else {
        offset_to_end: usize,
    },
    End,
    Input {
        prompt: Option<String>,
    },
    Roll {
        count: Expr,
        face: Expr,
    },
    Halt,
    Ill,
    Break,
    EnableWait,
    DisableWait,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub insts: Vec<Insts>,
    pub subs: std::collections::HashMap<String, usize>,
}

struct WaitsEnd {
    kind: Insts,
    index: usize,
}

macro_rules! die {
    ($( $x:expr ),*) => {
        eprintln!($($x,)*);
        std::process::exit(1);
    }
}

macro_rules! die_cont {
    ($msg: expr, $i: expr, $lexed: ident) => {
        die!(
            "Error: {}\n{}",
            $msg,
            $lexed.generate_loc_info(&$lexed.tokens[$i].loc)
        );
    };
}

macro_rules! expects {
    ($msg: expr, $($pat: pat)|+, $i: ident, $lexed: ident) => {
        if $lexed.tokens.len() <= $i || !matches!(&$lexed.tokens[$i].item, $($pat)|+) {
            die_cont!($msg, $i, $lexed);
        }
        $i += 1;
    };
}

macro_rules! parse_expr {
    ($($pat: pat)|+, $i: ident, $tks: ident, $lexed: ident) => {
        {
            let mut j = $i;
            while j < $tks.len()
                && !matches!(
                    $tks[j].item,
                    $($pat)|+
                )
            {
                j += 1;
            }
            let expr = Expr::from_tokens(&$tks[$i..j]).unwrap_or_else(|e| {
                use exprs::Error;
                match e {
                    Error::EmptyExpr => {
                        die_cont!("Expr is empty", $i, $lexed);
                    }
                    Error::InvalidToken(tk) => {
                        die!("Error: {}\n{}", "Failed to parse expr because of this token", $lexed.generate_loc_info(&tk.loc));
                    }
                }
            });
            $i = j;
            expr
        }
    }
}

pub fn parse(lexed: crate::lex::Lexed) -> Program {
    use lex::{Item, Keywords};
    let mut insts = vec![Insts::Ill];
    let mut waits_end_stack: Vec<WaitsEnd> = vec![]; // stmts waiting for End
    let mut subs = std::collections::HashMap::new(); // subroutines defined
    let mut i = 0;
    let tks = &lexed.tokens;
    while i < tks.len() {
        match &tks[i].item {
            Item::Inst(inst) => {
                match inst {
                    lex::Insts::Print => {
                        i += 1;
                        let mut args = Vec::new();
                        while i < tks.len() {
                            match &tks[i].item {
                                Item::Semi => break,
                                Item::Comma => {
                                    i += 1;
                                }
                                Item::Str(s) => {
                                    args.push(PrintArgs::Str(s.clone()));
                                    i += 1;
                                }
                                _ => args.push(PrintArgs::Expr(parse_expr!(
                                    Item::Comma | Item::Semi,
                                    i,
                                    tks,
                                    lexed
                                ))),
                            }
                        }

                        expects!("Semicolon expected", Item::Semi, i, lexed);
                        insts.push(Insts::Print { args })
                    }
                    lex::Insts::Sub => {
                        i += 1;
                        // check if the Sub is nested (which is not allowed)
                        if let Some(i) = waits_end_stack.last() {
                            if let Insts::Sub { .. } = i.kind {
                                die!("Semantic error: you cannot nest Sub.");
                            }
                        }

                        // register the sub to the name table
                        match &tks[i].item {
                            Item::Ident(name) => {
                                i += 1;
                                expects!("Semicolon expected", Item::Semi, i, lexed);
                                if subs.insert(name.clone(), insts.len()).is_some() {
                                    die!(
                                        "Semantic error: subroutine name \"{}\" is conflicting",
                                        name
                                    );
                                }
                                let inst_obj = Insts::Sub {
                                    name: name.clone(),
                                    offset_to_end: 0,
                                };
                                waits_end_stack.push(WaitsEnd {
                                    kind: inst_obj.clone(),
                                    index: insts.len(),
                                });
                                insts.push(inst_obj);
                            }
                            _ => {
                                die!("Semantic error: subroutine name not found");
                            }
                        }
                    }
                    lex::Insts::Call => {
                        i += 1;
                        match &tks[i].item {
                            Item::Ident(name) => {
                                i += 1;
                                expects!("Semicolon expected", Item::Semi, i, lexed);
                                insts.push(Insts::Call { name: name.clone() });
                            }
                            _ => {
                                die!("Semantic error: subroutine name not found");
                            }
                        }
                    }
                    lex::Insts::While => {
                        i += 1;
                        let inst_obj = Insts::While {
                            cond: parse_expr!(Item::Semi, i, tks, lexed),
                            offset_to_end: 0,
                        };
                        expects!("Semicolon expected", Item::Semi, i, lexed);
                        waits_end_stack.push(WaitsEnd {
                            kind: inst_obj.clone(),
                            index: insts.len(),
                        });
                        insts.push(inst_obj);
                    }
                    lex::Insts::Let => {
                        i += 1;
                        match &tks[i].item {
                            Item::Ident(name) => {
                                i += 1;
                                if name.starts_with("_") {
                                    die!("Semantic error: Identifier starts with _ is reserved");
                                }
                                expects!("\"Be\" expected", Item::Key(Keywords::Be), i, lexed);

                                let init = parse_expr!(
                                    Item::Semi | Item::Key(Keywords::AsMut),
                                    i,
                                    tks,
                                    lexed
                                );

                                expects!(
                                    "\"AsMut\" or semicolon expected",
                                    Item::Semi | Item::Key(Keywords::AsMut),
                                    i,
                                    lexed
                                );

                                let is_mut = {
                                    if tks[i - 1].item == Item::Key(Keywords::AsMut) {
                                        expects!("Semicolon expected", Item::Semi, i, lexed);
                                        true
                                    } else {
                                        false
                                    }
                                };

                                insts.push(Insts::Let {
                                    name: name.clone(),
                                    init,
                                    is_mut,
                                });
                            }
                            _ => {
                                die!("Semantic error: ident expected");
                            }
                        }
                    }
                    lex::Insts::Modify => {
                        i += 1;
                        match &tks[i].item {
                            Item::Ident(name) => {
                                i += 1;
                                if name.starts_with("_") {
                                    die!("Semantic error: Identifier starts with _ is reserved and cannot be modified");
                                }

                                expects!("To expected", Item::Key(Keywords::To), i, lexed);

                                let expr = parse_expr!(Item::Semi, i, tks, lexed);
                                expects!("Semicolon expected", Item::Semi, i, lexed);

                                insts.push(Insts::Modify {
                                    name: name.clone(),
                                    expr,
                                });
                            }
                            _ => {
                                die!("Semantic error: ident expected");
                            }
                        }
                    }
                    lex::Insts::If => {
                        i += 1;

                        let cond = parse_expr!(Item::Semi, i, tks, lexed);
                        expects!("Semicolon expected", Item::Semi, i, lexed);

                        let inst_obj = Insts::If {
                            cond,
                            offset_to_next: 0,
                        };
                        waits_end_stack.push(WaitsEnd {
                            kind: inst_obj.clone(),
                            index: insts.len(),
                        });
                        insts.push(inst_obj);
                    }
                    lex::Insts::ElIf => {
                        let prev = waits_end_stack.pop().unwrap_or_else(|| {
                            die!("Semantic error: a stray ElIf detected.");
                        });

                        let offset_to_next = insts.len() - prev.index;
                        insts[prev.index] = match prev.kind {
                            Insts::If { cond, .. } => Insts::If {
                                cond: cond.clone(),
                                offset_to_next,
                            },
                            Insts::ElIf { cond, .. } => Insts::ElIf {
                                cond: cond.clone(),
                                offset_to_next,
                            },
                            _ => {
                                die!("Semantic error: cannot find corresponding Element for ElIf");
                            }
                        };

                        i += 1;

                        let cond = parse_expr!(Item::Semi, i, tks, lexed);
                        expects!("Semicolon expected", Item::Semi, i, lexed);

                        let inst_obj = Insts::ElIf {
                            cond,
                            offset_to_next: 0,
                        };
                        waits_end_stack.push(WaitsEnd {
                            kind: inst_obj.clone(),
                            index: insts.len(),
                        });
                        insts.push(inst_obj);
                    }
                    lex::Insts::Else => {
                        i += 1;
                        expects!("Semicolon expected", Item::Semi, i, lexed);
                        let prev = waits_end_stack.pop().unwrap_or_else(|| {
                            die!("Semantic error: a stray Else detected.");
                        });
                        let offset_to_next = insts.len() - prev.index;
                        insts[prev.index] = match prev.kind {
                            Insts::If { cond, .. } => Insts::If {
                                cond: cond.clone(),
                                offset_to_next,
                            },
                            Insts::ElIf { cond, .. } => Insts::ElIf {
                                cond: cond.clone(),
                                offset_to_next,
                            },
                            _ => {
                                die!("Semantic error: cannot find corresponding Element for Else");
                            }
                        };
                        let inst_obj = Insts::Else { offset_to_end: 0 };
                        waits_end_stack.push(WaitsEnd {
                            kind: inst_obj.clone(),
                            index: insts.len(),
                        });
                        insts.push(inst_obj);
                    }
                    lex::Insts::End => {
                        i += 1;
                        expects!("Semicolon expected", Item::Semi, i, lexed);
                        let start = waits_end_stack.pop().unwrap_or_else(|| {
                            die!("Semantic error: a stray End detected.");
                        });
                        let offset_to_end = insts.len() - start.index;
                        insts[start.index] = match start.kind {
                            Insts::Sub { name, .. } => Insts::Sub {
                                name,
                                offset_to_end,
                            },
                            Insts::While { cond, .. } => Insts::While {
                                cond,
                                offset_to_end,
                            },
                            Insts::If { ref cond, .. } => Insts::If {
                                cond: cond.clone(),
                                offset_to_next: offset_to_end,
                            },
                            Insts::ElIf { ref cond, .. } => Insts::ElIf {
                                cond: cond.clone(),
                                offset_to_next: offset_to_end,
                            },
                            Insts::Else { .. } => Insts::Else { offset_to_end },
                            other => {
                                die!("Semantic error: cannot End {:?}", other);
                            }
                        };

                        insts.push(Insts::End);
                    }
                    lex::Insts::Input => {
                        i += 1;
                        insts.push(Insts::Input {
                            prompt: if let Item::Str(prompt) = &tks[i].item {
                                i += 1;
                                Some(prompt.clone())
                            } else {
                                None
                            },
                        });
                        expects!("Semicolon expected", Item::Semi, i, lexed);
                    }
                    lex::Insts::Roll => {
                        i += 1;

                        let count = parse_expr!(Item::Key(Keywords::Dice), i, tks, lexed);
                        expects!("\"Dice\" expected", Item::Key(Keywords::Dice), i, lexed);

                        let face = parse_expr!(Item::Key(Keywords::Face), i, tks, lexed);
                        expects!("\"Face\" expected", Item::Key(Keywords::Face), i, lexed);

                        insts.push(Insts::Roll { count, face });
                    }
                    lex::Insts::Halt => {
                        i += 1;
                        expects!("Semicolon expected", Item::Semi, i, lexed);
                        insts.push(Insts::Halt)
                    }
                    lex::Insts::Break => {
                        i += 1;
                        expects!("Semicolon expected", Item::Semi, i, lexed);
                        insts.push(Insts::Break)
                    }
                    lex::Insts::EnableWait => {
                        i += 1;
                        expects!("Semicolon expected", Item::Semi, i, lexed);
                        insts.push(Insts::EnableWait)
                    }
                    lex::Insts::DisableWait => {
                        i += 1;
                        expects!("Semicolon expected", Item::Semi, i, lexed);
                        insts.push(Insts::DisableWait)
                    }
                }
            }
            _ => {
                die_cont!("Line must begin with inst", i, lexed);
            }
        }
    }
    Program { insts, subs }
}
