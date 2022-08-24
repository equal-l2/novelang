use crate::exprs::Expr;
use crate::parse;
use crate::span::Span;
use crate::IdentName;

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: crate::span::Span,
}

#[derive(Debug)]
pub enum ErrorKind {
    NoEnd,
    ExtraEnd,       // A stray End detected.
    NotInALoop,     // Break/Continue must be in a loop
    NoPairIfOrElif, // A stray Else/Else-If detected.
    NotInASub,      // Return must be in a Sub
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::NoEnd => write!(f, "This statement has no pair End statement"),
            ErrorKind::ExtraEnd => write!(f, "This End statement has no starting statement"),
            ErrorKind::NotInALoop => write!(f, "This statement must be in a loop (While/For)"),
            ErrorKind::NoPairIfOrElif => write!(f, "This statement has no pair If/Else If"),
            ErrorKind::NotInASub => write!(f, "This statement must be in a Sub"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Normal(parse::NormalStmt),
    Block(BlockStmt),
    Ill,
}

#[derive(Debug, Clone)]
pub enum BlockStmt {
    For {
        counter: IdentName,
        from: Expr,
        to: Expr,
        offset_to_end: usize,
    },
    While {
        cond: Expr,
        offset_to_end: usize,
    },
    Break,
    Continue,
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
    Sub {
        name: IdentName,
        offset_to_end: usize,
    },
    Return,
    End,
}

#[derive(Debug, Clone)]
pub struct BlockChecked {
    pub stmts: Vec<Statement>,
}

#[derive(Clone)]
enum ScopeKind {
    Loop,
    Sub,
    Other,
}

// TODO: have the starting stmt as a reference to provide proper error message
struct Scope {
    kind: ScopeKind,
    start_span: Span,
    ret_idx: usize,
}

impl Scope {
    fn new(kind: ScopeKind, start_span: Span, ret_idx: usize) -> Self {
        Self {
            kind,
            start_span,
            ret_idx,
        }
    }
}

struct ScopeStack {
    scopes: Vec<Scope>,
}

impl ScopeStack {
    fn new() -> Self {
        let internals = Scope::new(ScopeKind::Other, Span::default(), 0);
        Self {
            scopes: vec![internals],
        }
    }

    fn kinds(&self) -> Vec<&ScopeKind> {
        self.scopes.iter().map(|sc| &sc.kind).rev().collect()
    }

    fn push(&mut self, kind: ScopeKind, span: Span, ret_idx: usize) {
        self.scopes.push(Scope::new(kind, span, ret_idx));
    }

    fn pop(&mut self) -> Option<usize> {
        if self.scopes.len() > 1 {
            let sc = self.scopes.pop().unwrap();
            Some(sc.ret_idx)
        } else {
            // global scope cannot be pop
            None
        }
    }
}

pub fn check_block(parsed: crate::parse::Parsed) -> Result<BlockChecked, Vec<Error>> {
    let mut stmts = Vec::<Statement>::new();
    let mut scope_stack = ScopeStack::new();
    let mut errors = vec![];

    for parsed_stmt in parsed.stmts {
        let stmt = match parsed_stmt {
            parse::Statement::Normal(normal) => Statement::Normal(normal),
            parse::Statement::Block(block, span) => match block {
                parse::BlockStmt::For { counter, from, to } => {
                    // "For" <name> "from" <expr> "to" <expr> ";"
                    scope_stack.push(ScopeKind::Loop, span, stmts.len());
                    Statement::Block(BlockStmt::For {
                        counter,
                        from,
                        to,
                        offset_to_end: 0,
                    })
                }
                parse::BlockStmt::While { cond } => {
                    scope_stack.push(ScopeKind::Loop, span, stmts.len());
                    Statement::Block(BlockStmt::While {
                        cond,
                        offset_to_end: 0,
                    })
                }
                parse::BlockStmt::Break => {
                    let mut loop_found = false;
                    for k in scope_stack.kinds() {
                        match k {
                            ScopeKind::Loop => {
                                loop_found = true;
                                break;
                            }
                            ScopeKind::Sub => break,
                            _ => {}
                        }
                    }

                    if !loop_found {
                        errors.push(Error {
                            kind: ErrorKind::NotInALoop,
                            span,
                        });
                    }
                    Statement::Block(BlockStmt::Break)
                }
                parse::BlockStmt::Continue => {
                    // "Continue" ";"
                    let mut loop_found = false;
                    for k in scope_stack.kinds() {
                        match k {
                            ScopeKind::Loop => {
                                loop_found = true;
                                break;
                            }
                            ScopeKind::Sub => break,
                            _ => {}
                        }
                    }

                    if !loop_found {
                        errors.push(Error {
                            kind: ErrorKind::NotInALoop,
                            span,
                        });
                    }
                    Statement::Block(BlockStmt::Continue)
                }
                parse::BlockStmt::If { cond } => {
                    scope_stack.push(ScopeKind::Other, span, stmts.len());

                    Statement::Block(BlockStmt::If {
                        cond,
                        offset_to_next: 0,
                    })
                }
                parse::BlockStmt::ElIf { cond } => {
                    let prev_idx = scope_stack.pop();

                    if let Some(prev_idx) = prev_idx {
                        let offset_to_next = stmts.len() - prev_idx;

                        let prev = stmts[prev_idx].clone();
                        stmts[prev_idx] = match prev {
                            Statement::Block(BlockStmt::If { cond, .. }) => {
                                Statement::Block(BlockStmt::If {
                                    cond: cond.clone(),
                                    offset_to_next,
                                })
                            }
                            Statement::Block(BlockStmt::ElIf { cond, .. }) => {
                                Statement::Block(BlockStmt::ElIf {
                                    cond: cond.clone(),
                                    offset_to_next,
                                })
                            }
                            _ => {
                                // TODO: how can this happen?
                                panic!("Invalid statement was finding Else-If");
                            }
                        };
                    } else {
                        errors.push(Error {
                            kind: ErrorKind::NoPairIfOrElif,
                            span: span.clone(),
                        });
                    }

                    scope_stack.push(ScopeKind::Other, span, stmts.len());
                    Statement::Block(BlockStmt::ElIf {
                        cond,
                        offset_to_next: 0,
                    })
                }
                parse::BlockStmt::Else => {
                    let prev_idx = scope_stack.pop();

                    if let Some(prev_idx) = prev_idx {
                        let offset_to_next = stmts.len() - prev_idx;

                        let prev = stmts[prev_idx].clone();
                        stmts[prev_idx] = match prev {
                            Statement::Block(BlockStmt::If { cond, .. }) => {
                                Statement::Block(BlockStmt::If {
                                    cond: cond.clone(),
                                    offset_to_next,
                                })
                            }
                            Statement::Block(BlockStmt::ElIf { cond, .. }) => {
                                Statement::Block(BlockStmt::ElIf {
                                    cond: cond.clone(),
                                    offset_to_next,
                                })
                            }
                            _ => {
                                // TODO: proper error handle
                                panic!("Invalid statement was finding Else");
                            }
                        };
                    } else {
                        errors.push(Error {
                            kind: ErrorKind::NoPairIfOrElif,
                            span: span.clone(),
                        });
                    }

                    scope_stack.push(ScopeKind::Other, span, stmts.len());
                    Statement::Block(BlockStmt::Else { offset_to_end: 0 })
                }
                parse::BlockStmt::Sub { name } => {
                    // create new scope
                    scope_stack.push(ScopeKind::Sub, span, stmts.len());

                    Statement::Block(BlockStmt::Sub {
                        name: name.clone(),
                        offset_to_end: 0,
                    })
                }
                parse::BlockStmt::Return => {
                    // "Return" ";"
                    let mut sub_found = false;
                    for k in scope_stack.kinds() {
                        if let ScopeKind::Sub = k {
                            sub_found = true;
                            break;
                        }
                    }

                    if !sub_found {
                        errors.push(Error {
                            kind: ErrorKind::NotInASub,
                            span,
                        });
                    }
                    Statement::Block(BlockStmt::Return)
                }
                parse::BlockStmt::End => {
                    // Pop stack and assign end index
                    let prev_idx = scope_stack.pop();

                    if let Some(prev_idx) = prev_idx {
                        let offset_to_end = stmts.len() - prev_idx;

                        let prev = stmts[prev_idx].clone();
                        stmts[prev_idx] = match prev {
                            Statement::Block(BlockStmt::Sub { name, .. }) => {
                                Statement::Block(BlockStmt::Sub {
                                    name,
                                    offset_to_end,
                                })
                            }
                            Statement::Block(BlockStmt::While { cond, .. }) => {
                                Statement::Block(BlockStmt::While {
                                    cond,
                                    offset_to_end,
                                })
                            }
                            Statement::Block(BlockStmt::If { ref cond, .. }) => {
                                Statement::Block(BlockStmt::If {
                                    cond: cond.clone(),
                                    offset_to_next: offset_to_end,
                                })
                            }
                            Statement::Block(BlockStmt::ElIf { ref cond, .. }) => {
                                Statement::Block(BlockStmt::ElIf {
                                    cond: cond.clone(),
                                    offset_to_next: offset_to_end,
                                })
                            }
                            Statement::Block(BlockStmt::Else { .. }) => {
                                Statement::Block(BlockStmt::Else { offset_to_end })
                            }
                            Statement::Block(BlockStmt::For {
                                counter, from, to, ..
                            }) => Statement::Block(BlockStmt::For {
                                counter,
                                from,
                                to,
                                offset_to_end,
                            }),
                            _ => {
                                // TODO: proper error handling
                                panic!("Invalid statement was finding End");
                            }
                        };
                    } else {
                        errors.push(Error {
                            kind: ErrorKind::ExtraEnd,
                            span,
                        });
                    }

                    Statement::Block(BlockStmt::End)
                }
            },
            parse::Statement::Ill => Statement::Ill,
        };
        stmts.push(stmt);
    }

    // Convert scopes left into errors
    // Note the global scope should be ignored
    for scope in scope_stack.scopes.drain(1..) {
        errors.push(Error {
            kind: ErrorKind::NoEnd,
            span: scope.start_span,
        });
    }

    if errors.is_empty() {
        Ok(BlockChecked { stmts })
    } else {
        Err(errors)
    }
}
