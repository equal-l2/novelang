use crate::exprs::Expr;

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
    ($msg: expr, $loc: expr, $lex: ident) => {
        die!("Error: {}\n{}", $msg, $lex.generate_src_loc(&$loc));
    };
}

macro_rules! expects {
    ($msg: expr, $item: path, $i: ident, $lex: ident) => {
        if $lex.tokens.len() <= $i || $lex.tokens[$i].item != $item {
            die_cont!($msg, $lex.tokens[$i].loc, $lex);
        }
        $i += 1;
    };
}

pub fn parse(lexed: crate::lex::Lexed) -> Program {
    use crate::lex::{self, Item};
    let mut insts = vec![Insts::Ill];
    let mut waits_end_stack: Vec<WaitsEnd> = vec![]; // stmts waiting for End
    let mut subs = std::collections::HashMap::new(); // subroutines defined
    let mut i = 0;
    let tks = &lexed.tokens;
    while i < tks.len() {
        match dbg!(&tks[i].item) {
            Item::Inst(inst) => {
                match inst {
                    lex::Insts::Print => {
                        i += 1;
                        let mut args = Vec::new();
                        while i < tks.len() && tks[i].item != Item::Semi {
                            args.push(match &tks[i].item {
                                Item::Str(s) => PrintArgs::Str(s.clone()),
                                _ => {
                                    let orig_i = i;
                                    while i < tks.len()
                                        && matches!(
                                            tks[i].item,
                                            Item::Ident(_) | Item::Num(_) | Item::Ari(_) | Item::Rel(_)
                                        )
                                    {
                                        i += 1;
                                    }
                                    PrintArgs::Expr(
                                        Expr::from_tokens(&tks[orig_i..i]).unwrap_or_else(|tk| {
                                            die_cont!("Failed to parse expr", tk.loc, lexed);
                                        }),
                                    )
                                }
                            });
                            i += 1;
                        }
                        expects!("Semicolon expected", lex::Item::Semi, i, lexed);
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
                                expects!("Semicolon expected", lex::Item::Semi, i, lexed);
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
                                expects!("Semicolon expected", lex::Item::Semi, i, lexed);
                                insts.push(Insts::Call { name: name.clone() });
                            }
                            _ => {
                                die!("Semantic error: subroutine name not found");
                            }
                        }
                    }
                    lex::Insts::While => {
                        i += 1;
                        let orig_i = i;
                        while i < tks.len() && tks[i].item != lex::Item::Semi {
                            i += 1;
                        }
                        if tks[i].item != lex::Item::Semi {
                            die_cont!("Semicolon expected", tks[i].loc, lexed);
                        }
                        let inst_obj = Insts::While {
                            cond: Expr::from_tokens(&tks[orig_i..i]).unwrap_or_else(|tk| {
                                die_cont!("Failed to parse expr", tk.loc, lexed);
                            }),
                            offset_to_end: 0,
                        };
                        i += 1;
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

                                {
                                    let be = lex::Item::Key(lex::Keywords::Be);
                                    expects!("Be expected", be, i, lexed);
                                }

                                let orig_i = i;
                                while i < tks.len()
                                    && !matches!(
                                        tks[i].item,
                                        lex::Item::Semi | lex::Item::Key(lex::Keywords::AsMut)
                                    )
                                {
                                    i += 1;
                                }
                                if !matches!(
                                    tks[i].item,
                                    lex::Item::Semi | lex::Item::Key(lex::Keywords::AsMut)
                                ) {
                                    die_cont!("AsMut or Semicolon expected", tks[i].loc, lexed);
                                }
                                let init =
                                    Expr::from_tokens(&tks[orig_i..i]).unwrap_or_else(|tk| {
                                        die_cont!("Failed to parse expr", tk.loc, lexed);
                                    });
                                let is_mut = {
                                    if tks[i].item == lex::Item::Key(lex::Keywords::AsMut) {
                                        i += 1;
                                        true
                                    } else {
                                        false
                                    }
                                };
                                expects!("Semicolon expected", lex::Item::Semi, i, lexed);
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

                                let orig_i = i;
                                while i < tks.len()
                                    && tks[i].item != lex::Item::Key(lex::Keywords::To)
                                {
                                    i += 1;
                                }

                                if tks[i].item != lex::Item::Key(lex::Keywords::To) {
                                    die_cont!("To expected", tks[i].loc, lexed);
                                }
                                i += 1;

                                let expr =
                                    Expr::from_tokens(&tks[orig_i..i]).unwrap_or_else(|tk| {
                                        die_cont!("Failed to parse expr", tk.loc, lexed);
                                    });
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
                        let orig_i = i;
                        while i < tks.len() && tks[i].item != lex::Item::Semi {
                            i += 1;
                        }
                        if tks[i].item != lex::Item::Semi {
                            die_cont!("Semicolon expected", tks[i].loc, lexed);
                        }
                        let cond = Expr::from_tokens(&tks[orig_i..i]).unwrap_or_else(|tk| {
                            die_cont!("Failed to parse expr", tk.loc, lexed);
                        });
                        i += 1;
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
                        let orig_i = i;
                        while i < tks.len() && tks[i].item != lex::Item::Semi {
                            i += 1;
                        }
                        if tks[i].item != lex::Item::Semi {
                            die_cont!("Semicolon expected", tks[i].loc, lexed);
                        }
                        let cond = Expr::from_tokens(&tks[orig_i..i]).unwrap_or_else(|tk| {
                            die_cont!("Failed to parse expr", tk.loc, lexed);
                        });
                        i += 1;

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
                        expects!("Semicolon expected", lex::Item::Semi, i, lexed);
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
                        expects!("Semicolon expected", lex::Item::Semi, i, lexed);
                    }
                    lex::Insts::Roll => {
                        i += 1;
                        let orig_i = i;
                        while i < tks.len() && tks[i].item != lex::Item::Key(lex::Keywords::Dice) {
                            i += 1;
                        }
                        if tks[i].item != lex::Item::Key(lex::Keywords::Dice) {
                            die_cont!("Dice expected", tks[i].loc, lexed);
                        }
                        let count = Expr::from_tokens(&tks[orig_i..i]).unwrap_or_else(|tk| {
                            die_cont!("Failed to parse expr", tk.loc, lexed);
                        });
                        i += 1;

                        let orig_i = i;
                        while i < tks.len() && tks[i].item != lex::Item::Key(lex::Keywords::Face) {
                            i += 1;
                        }
                        if tks[i].item != lex::Item::Key(lex::Keywords::Face) {
                            die_cont!("Face expected", tks[i].loc, lexed);
                        }
                        let face = Expr::from_tokens(&tks[orig_i..i]).unwrap_or_else(|tk| {
                            die_cont!("Failed to parse expr", tk.loc, lexed);
                        });
                        i += 1;

                        insts.push(Insts::Roll { count, face });
                    }
                    lex::Insts::Halt => {
                        i += 1;
                        expects!("Semicolon expected", lex::Item::Semi, i, lexed);
                        insts.push(Insts::Halt)
                    }
                    lex::Insts::Break => {
                        i += 1;
                        expects!("Semicolon expected", lex::Item::Semi, i, lexed);
                        insts.push(Insts::Break)
                    }
                    lex::Insts::EnableWait => {
                        i += 1;
                        expects!("Semicolon expected", lex::Item::Semi, i, lexed);
                        insts.push(Insts::EnableWait)
                    }
                    lex::Insts::DisableWait => {
                        i += 1;
                        expects!("Semicolon expected", lex::Item::Semi, i, lexed);
                        insts.push(Insts::DisableWait)
                    }
                }
            }
            _ => {
                die_cont!("Line must begin with inst", tks[i].loc, lexed);
            }
        }
    }
    Program { insts, subs }
}
