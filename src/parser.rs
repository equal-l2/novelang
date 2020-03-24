use crate::exprs::*;
use pest::Parser;

macro_rules! die {
    ($( $x:expr ),*) => {
        eprintln!($($x,)*);
        std::process::exit(1);
    }
}

#[derive(pest_derive::Parser)]
#[grammar = "prog.pest"]
struct ProgParser;

#[derive(Debug, Clone)]
pub enum PrintArgs {
    String(String),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Inst {
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
        cond: CompExpr,
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
        cond: CompExpr,
        offset_to_next: usize,
    },
    ElIf {
        cond: CompExpr,
        offset_to_next: usize,
    },
    Else {
        offset_to_end: usize,
    },
    End,
    Ill,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub insts: Vec<Inst>,
    pub subs: std::collections::HashMap<String, usize>,
}

struct WaitsEnd {
    kind: Inst,
    index: usize,
}

impl std::fmt::Display for WaitsEnd {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "{} at {}",
            match self.kind {
                Inst::Print { .. } => "Print",
                Inst::Sub { .. } => "Sub",
                Inst::Call { .. } => "Call",
                Inst::While { .. } => "While",
                Inst::Let { .. } => "Let",
                Inst::Modify { .. } => "Modify",
                Inst::If { .. } => "If",
                Inst::ElIf { .. } => "ElIf",
                Inst::Else { .. } => "Else",
                Inst::End => "End",
                Inst::Ill => "Ill"
            },
            self.index,
        )
    }
}

pub fn parse(s: &str) -> Option<Program> {
    let lines = ProgParser::parse(Rule::Prog, s);
    if let Err(e) = lines {
        eprintln!("{}", e);
        return None;
    }
    let stmts = lines.unwrap();

    let mut insts = vec![Inst::Ill];
    let mut waits_end_stack: Vec<WaitsEnd> = vec![]; // stmts waiting for End
    let mut subs = std::collections::HashMap::new(); // subroutines defined
    for stmt in stmts {
        match stmt.as_rule() {
            Rule::Print => insts.push(Inst::Print {
                args: stmt
                    .into_inner()
                    .map(|s| match s.as_rule() {
                        Rule::StringContent => PrintArgs::String(s.as_str().to_owned()),
                        Rule::Expr => PrintArgs::Expr(Expr::parse_stmt(s)),
                        other => {
                            die!("Semantic error: unexpected rule : {:?}", other);
                        }
                    })
                    .collect(),
            }),
            Rule::Sub => {
                // check if the Sub is nested (which is not allowed)
                if let Some(i) = waits_end_stack.last() {
                    if let Inst::Sub { .. } = i.kind {
                        die!("Semantic error: you cannot nest Sub.");
                    }
                }

                // register the sub to the name table
                let fn_name = stmt.into_inner().as_str().to_owned();
                if subs.insert(fn_name.clone(), insts.len()).is_some() {
                    die!(
                        "Semantic error: function name \"{}\" is conflicting",
                        fn_name
                    );
                }

                let inst_obj = Inst::Sub {
                    name: fn_name,
                    offset_to_end: 0,
                };
                waits_end_stack.push(WaitsEnd {
                    kind: inst_obj.clone(),
                    index: insts.len(),
                });
                insts.push(inst_obj);
            }
            Rule::Call => insts.push(Inst::Call {
                name: stmt.into_inner().as_str().to_owned(),
            }),
            Rule::While => {
                let inst_obj = Inst::While {
                    cond: CompExpr::parse_stmt(stmt.into_inner().next().unwrap()),
                    offset_to_end: 0,
                };
                waits_end_stack.push(WaitsEnd {
                    kind: inst_obj.clone(),
                    index: insts.len(),
                });
                insts.push(inst_obj);
            }
            Rule::Let => {
                let mut it = stmt.into_inner();
                let name = it.next().unwrap().as_str().to_owned();
                let init = Expr::parse_stmt(it.next().unwrap());
                let is_mut = it.next().is_some();

                insts.push(Inst::Let { name, init, is_mut });
            }
            Rule::Modify => {
                let mut it = stmt.into_inner();
                insts.push(Inst::Modify {
                    name: it.next().unwrap().as_str().to_owned(),
                    expr: Expr::parse_stmt(it.next().unwrap()),
                });
            }
            Rule::If => {
                let inst_obj = Inst::If {
                    cond: CompExpr::parse_stmt(stmt.into_inner().next().unwrap()),
                    offset_to_next: 0,
                };
                waits_end_stack.push(WaitsEnd {
                    kind: inst_obj.clone(),
                    index: insts.len(),
                });
                insts.push(inst_obj);
            }
            Rule::ElIf => {
                let prev = waits_end_stack.pop().unwrap_or_else(|| {
                    die!("Semantic error: a stray ElIf detected.");
                });

                let offset_to_next = insts.len() - prev.index;
                insts[prev.index] = match prev.kind {
                    Inst::If { cond, .. } => Inst::If {
                        cond: cond.clone(),
                        offset_to_next,
                    },
                    Inst::ElIf { cond, .. } => Inst::ElIf {
                        cond: cond.clone(),
                        offset_to_next,
                    },
                    _ => {
                        die!("Semantic error: cannot find corresponding Element for ElIf");
                    }
                };

                let inst_obj = Inst::ElIf {
                    cond: CompExpr::parse_stmt(stmt.into_inner().next().unwrap()),
                    offset_to_next: 0,
                };
                waits_end_stack.push(WaitsEnd {
                    kind: inst_obj.clone(),
                    index: insts.len(),
                });
                insts.push(inst_obj);
            }
            Rule::Else => {
                let prev = waits_end_stack.pop().unwrap_or_else(|| {
                    die!("Semantic error: a stray Else detected.");
                });
                let offset_to_next = insts.len() - prev.index;
                insts[prev.index] = match prev.kind {
                    Inst::If { cond, .. } => Inst::If {
                        cond: cond.clone(),
                        offset_to_next,
                    },
                    Inst::ElIf { cond, .. } => Inst::ElIf {
                        cond: cond.clone(),
                        offset_to_next,
                    },
                    _ => {
                        die!("Semantic error: cannot find corresponding Element for Else");
                    }
                };
                let inst_obj = Inst::Else { offset_to_end: 0 };
                waits_end_stack.push(WaitsEnd {
                    kind: inst_obj.clone(),
                    index: insts.len(),
                });
                insts.push(inst_obj);
            }
            Rule::End => {
                let start = waits_end_stack.pop().unwrap_or_else(|| {
                    die!("Semantic error: a stray End detected.");
                });
                let offset_to_end = insts.len() - start.index;
                insts[start.index] = match start.kind {
                    Inst::Sub { name, .. } => Inst::Sub {
                        name,
                        offset_to_end,
                    },
                    Inst::While { cond, .. } => Inst::While {
                        cond,
                        offset_to_end,
                    },
                    Inst::If { ref cond, .. } => Inst::If {
                        cond: cond.clone(),
                        offset_to_next: offset_to_end,
                    },
                    Inst::ElIf { ref cond, .. } => Inst::ElIf {
                        cond: cond.clone(),
                        offset_to_next: offset_to_end,
                    },
                    Inst::Else { .. } => Inst::Else { offset_to_end },
                    other => {
                        die!("Semantic error: cannot End {:?}", other);
                    }
                };

                insts.push(Inst::End);
            }
            Rule::EOI => break,
            Rule::Comment => {}
            other => {
                die!("Semantic error: unexpected rule : {:?}", other);
            }
        }
    }
    Some(Program { insts, subs })
}
