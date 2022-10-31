use crate::exprs::Expr;
use crate::parse;

#[derive(Debug, Clone)]
pub enum Statement {
    Normal(parse::NormalStmt),
    Block(BlockStmt),
    Ill,
}

pub type BlockList = Vec<Statement>;

#[derive(Debug, Clone)]
pub struct Conditional {
    pub cond: Expr,
    pub body: BlockList,
}

use crate::target::Ident;
use parse::stmt::sub::*;

#[derive(Debug, Clone)]
pub struct For {
    pub counter: Ident,
    pub from: Expr,
    pub to: Expr,
    pub body: BlockList,
}

#[derive(Debug, Clone)]
pub struct If {
    pub branches: Vec<Conditional>,
    pub r#else: Option<BlockList>,
}

#[derive(Debug, Clone)]
pub struct Sub {
    pub name: SubName,
    pub args: Option<SubArgs>,
    pub res: Option<SubRes>,
    pub body: BlockList,
}

#[derive(Debug, Clone)]
pub struct While {
    pub cond: Expr,
    pub body: BlockList,
}

#[derive(Debug, Clone)]
pub enum BlockStmt {
    For(For),
    While(While),
    If(If),
    Sub(Sub),
}

#[derive(Debug, Clone)]
pub struct BlockParsed {
    pub blocks: BlockList,
}

use parse::Statement as ParseStmt;

trait TryFromStmts {
    fn try_from_stmts<'a, T>(stmts: &mut std::iter::Peekable<T>) -> Result<Self, ()>
    where
        Self: Sized,
        T: Iterator<Item = ParseStmt>;
}

impl TryFromStmts for BlockList {
    fn try_from_stmts<'a, T>(stmts: &mut std::iter::Peekable<T>) -> Result<Self, ()>
    where
        Self: Sized,
        T: Iterator<Item = ParseStmt>,
    {
        use parse::stmt::variant as parse_var;
        use parse::BlockStmt as ParseBlockStmt;
        let mut blocks = vec![];

        while let Some(stmt) = stmts.next() {
            blocks.push(match stmt {
                ParseStmt::Normal(stmt) => Statement::Normal(stmt),
                ParseStmt::Block(stmt, span) => Statement::Block(match stmt {
                    ParseBlockStmt::For(inner) => {
                        let parse_var::For { counter, from, to } = inner;
                        let body = BlockList::try_from_stmts(stmts)?;

                        // TODO: proper error handling
                        assert!(matches!(
                            stmts.next(),
                            Some(ParseStmt::Block(ParseBlockStmt::End, _))
                        ));

                        BlockStmt::For(For {
                            counter,
                            from,
                            to,
                            body,
                        })
                    }

                    ParseBlockStmt::While { cond } => {
                        let body = BlockList::try_from_stmts(stmts)?;

                        // TODO: proper error handling
                        assert!(matches!(
                            stmts.next(),
                            Some(ParseStmt::Block(ParseBlockStmt::End, _))
                        ));

                        BlockStmt::While(While { cond, body })
                    }

                    ParseBlockStmt::If { cond } => {
                        let mut branches = vec![];

                        // parse head If
                        let body = BlockList::try_from_stmts(stmts)?;
                        branches.push(Conditional { cond, body });

                        let mut r#else = None;
                        loop {
                            let tail = stmts.next();
                            match tail {
                                Some(ParseStmt::Block(inner, _span)) => match inner {
                                    ParseBlockStmt::ElIf { cond } => {
                                        let body = BlockList::try_from_stmts(stmts)?;
                                        branches.push(Conditional { cond, body })
                                    }
                                    ParseBlockStmt::Else => {
                                        // TODO: handle
                                        assert!(r#else.is_none());
                                        r#else = Some(BlockList::try_from_stmts(stmts)?);
                                    }
                                    ParseBlockStmt::End => {
                                        break BlockStmt::If(If { branches, r#else })
                                    }
                                    _ => todo!("error!"),
                                },
                                _ => todo!("error!"),
                            }
                        }
                    }

                    ParseBlockStmt::Sub(inner) => {
                        let parse_var::Sub { name, args, res } = inner;
                        let body = BlockList::try_from_stmts(stmts)?;

                        // TODO: proper error handling
                        assert!(matches!(
                            stmts.next(),
                            Some(ParseStmt::Block(ParseBlockStmt::End, _))
                        ));

                        BlockStmt::Sub(Sub {
                            name,
                            args,
                            res,
                            body,
                        })
                    }

                    ParseBlockStmt::End | ParseBlockStmt::ElIf { .. } | ParseBlockStmt::Else => {
                        // must be handled by the outer parser
                        return Ok(blocks);
                    }

                    _ => {
                        todo!("Invalid statements for this context")
                    }
                }),
                ParseStmt::Ill => Statement::Ill,
            })
        }

        Ok(blocks)
    }
}

pub fn parse_block(parsed: crate::parse::Parsed) -> Result<BlockParsed, ()> {
    let blocks = BlockList::try_from_stmts(&mut parsed.stmts.into_iter().peekable())
        .expect("error handling TODO");
    Ok(BlockParsed { blocks })
}

// TODO: move the diagnostics logics into semck
/*

#[derive(Debug)]
pub enum Error {
    NoEnd,
    ExtraEnd,       // A stray End detected.
    NotInALoop,     // Break/Continue must be in a loop
    NoPairIfOrElif, // A stray Else/Else-If detected.
    NotInASub,      // Return must be in a Sub
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::NoEnd => write!(f, "This statement has no pair End statement"),
            Error::ExtraEnd => write!(f, "This End statement has no starting statement"),
            Error::NotInALoop => write!(f, "This statement must be in a loop (While/For)"),
            Error::NoPairIfOrElif => write!(f, "This statement has no pair If/Else If"),
            Error::NotInASub => write!(f, "This statement must be in a Sub"),
        }
    }
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
    const fn new(kind: ScopeKind, start_span: Span, ret_idx: usize) -> Self {
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

pub fn parse_block(parsed: crate::parse::Parsed) -> Result<BlockChecked, Vec<(Error, Span)>> {
    let mut stmts = Vec::<Statement>::new();
    let mut scope_stack = ScopeStack::new();
    let mut errors = vec![];

    for parsed_stmt in parsed.stmts {
        let stmt = match parsed_stmt {
            parse::Statement::Normal(normal) => Statement::Normal(normal),
            parse::Statement::Block(block, span) => match block {
                parse::BlockStmt::For(r#for) => {
                    scope_stack.push(ScopeKind::Loop, span, stmts.len());
                    Statement::Block(BlockStmt::For {
                        r#for,
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
                        errors.push((Error::NotInALoop, span));
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
                        errors.push((Error::NotInALoop, span));
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
                        match prev {
                            Statement::Block(BlockStmt::If { cond, .. }) => {
                                stmts[prev_idx] = Statement::Block(BlockStmt::If {
                                    cond: cond.clone(),
                                    offset_to_next,
                                });
                            }
                            Statement::Block(BlockStmt::ElIf { cond, .. }) => {
                                stmts[prev_idx] = Statement::Block(BlockStmt::ElIf {
                                    cond: cond.clone(),
                                    offset_to_next,
                                });
                            }
                            _ => {
                                // example: "Else;Else If something;"
                                // the second Else-If pops the scope of the first Else
                                errors.push((Error::NoPairIfOrElif, span.clone()));
                            }
                        };
                    } else {
                        errors.push((Error::NoPairIfOrElif, span.clone()));
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
                        match prev {
                            Statement::Block(BlockStmt::If { cond, .. }) => {
                                stmts[prev_idx] = Statement::Block(BlockStmt::If {
                                    cond: cond.clone(),
                                    offset_to_next,
                                });
                            }
                            Statement::Block(BlockStmt::ElIf { cond, .. }) => {
                                stmts[prev_idx] = Statement::Block(BlockStmt::ElIf {
                                    cond: cond.clone(),
                                    offset_to_next,
                                });
                            }
                            _ => {
                                // example: "Else;Else;"
                                // the second Else pops the scope of the first Else
                                errors.push((Error::NoPairIfOrElif, span.clone()));
                            }
                        };
                    } else {
                        errors.push((Error::NoPairIfOrElif, span.clone()));
                    }

                    scope_stack.push(ScopeKind::Other, span, stmts.len());
                    Statement::Block(BlockStmt::Else { offset_to_end: 0 })
                }
                parse::BlockStmt::Sub(sub) => {
                    // create new scope
                    scope_stack.push(ScopeKind::Sub, span, stmts.len());

                    Statement::Block(BlockStmt::Sub {
                        sub: sub.clone(),
                        offset_to_end: 0,
                    })
                }
                parse::BlockStmt::Return(ret) => {
                    // "Return" ";"
                    let mut sub_found = false;
                    for k in scope_stack.kinds() {
                        if let ScopeKind::Sub = k {
                            sub_found = true;
                            break;
                        }
                    }

                    if !sub_found {
                        errors.push((Error::NotInASub, span));
                    }
                    Statement::Block(BlockStmt::Return(ret))
                }
                parse::BlockStmt::End => {
                    // Pop stack and assign end index
                    let prev_idx = scope_stack.pop();

                    if let Some(prev_idx) = prev_idx {
                        let offset_to_end = stmts.len() - prev_idx;

                        let prev = stmts[prev_idx].clone();
                        stmts[prev_idx] = match prev {
                            Statement::Block(BlockStmt::Sub { sub, .. }) => {
                                Statement::Block(BlockStmt::Sub { sub, offset_to_end })
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
                            Statement::Block(BlockStmt::For { r#for, .. }) => {
                                Statement::Block(BlockStmt::For {
                                    r#for,
                                    offset_to_end,
                                })
                            }
                            _ => {
                                // TODO: proper error handling
                                panic!("Invalid statement was finding End");
                            }
                        };
                    } else {
                        errors.push((Error::ExtraEnd, span));
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
        errors.push((Error::NoEnd, scope.start_span));
    }

    if errors.is_empty() {
        Ok(BlockChecked { stmts })
    } else {
        Err(errors)
    }
}
*/
