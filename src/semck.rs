use crate::exprs::Expr;
use crate::lval::LVal;
use crate::span::*;
use crate::IdentName;

mod exprs;
mod types;

#[macro_use]
mod utils;

use exprs::TypeError;
use types::Type;
use utils::*;

#[derive(Debug)]
enum ParseError {
    TypeError(TypeError),
}

impl From<TypeError> for ParseError {
    fn from(e: TypeError) -> Self {
        Self::TypeError(e)
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assert {
        mesg: Expr,
        cond: Expr,
    },
    Call {
        name: IdentName,
    },
    Halt,
    Input {
        prompt: Option<String>,
        target: LVal,
        as_num: bool,
    },
    Let {
        name: IdentName,
        init: Expr,
        is_mut: bool,
    },
    Modify {
        target: LVal,
        expr: Expr,
    },
    Print {
        args: Vec<Expr>,
    },
    Roll {
        count: Expr,
        face: Expr,
        target: LVal,
    },

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
    Ill,
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub stmts: Vec<Statement>,
}

#[derive(Clone, Debug)]
struct TypeInfo {
    ty: Type,
    is_mut: bool,
}

type VarMap = std::collections::HashMap<IdentName, TypeInfo>;

#[derive(Clone)]
enum ScopeKind {
    Loop,
    Sub,
    Other,
}

// TODO: have the starting stmt as a reference to provide proper error message
struct Scope {
    map: VarMap,
    kind: ScopeKind,
    ret_idx: usize,
}

impl Scope {
    fn new(kind: ScopeKind, ret_idx: usize) -> Self {
        Self {
            map: VarMap::new(),
            kind,
            ret_idx,
        }
    }

    fn add_var(&mut self, name: IdentName, var: TypeInfo) -> bool {
        let to_add = !self.map.contains_key(&name);
        if to_add {
            self.map.insert(name, var);
        }

        to_add
    }

    fn get_type_info(&self, name: &IdentName) -> Option<&TypeInfo> {
        self.map.get(name)
    }
}

struct ScopeStack {
    scopes: Vec<Scope>,
}

impl ScopeStack {
    fn new() -> Self {
        let mut internals = Scope::new(ScopeKind::Other, 0);
        internals.add_var(
            IdentName::from("_wait"),
            TypeInfo {
                ty: Type::Bool,
                is_mut: true,
            },
        );
        Self {
            scopes: vec![internals],
        }
    }

    fn kinds(&self) -> Vec<&ScopeKind> {
        self.scopes.iter().map(|sc| &sc.kind).rev().collect()
    }

    fn push(&mut self, kind: ScopeKind, ret_idx: usize) {
        self.scopes.push(Scope::new(kind, ret_idx));
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

    fn get_top_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn add_var(&mut self, name: IdentName, info: TypeInfo) -> bool {
        self.get_top_mut().add_var(name, info)
    }

    fn get_type_info(&self, lval: &LVal) -> Option<TypeInfo> {
        match lval {
            LVal::Scalar(s) => self
                .scopes
                .iter()
                .rev()
                .map(|m| m.get_type_info(s))
                .find(Option::is_some)
                .flatten()
                .cloned(),
            LVal::Vector(l, _) => {
                let v = self.get_type_info(l);
                match v {
                    Some(TypeInfo {
                        ty: Type::Arr(i),
                        is_mut,
                    }) => Some(TypeInfo {
                        ty: (*i).clone(),
                        is_mut,
                    }),
                    Some(_) => unreachable!(),
                    None => None,
                }
            }
        }
    }

    fn is_empty(&self) -> bool {
        self.scopes.len() == 1
    }
}

pub struct Error(pub String, pub Span);

// TODO: move block analysis (while, for, if&else, sub) to the separate module `parse_block` (after parse, before semck)
pub fn check_semantics(parsed: crate::parse::Parsed) -> Result<Ast, Vec<Error>> {
    use crate::parse::{BlockStmt, NormalStmt, Statement as ParsedStmt};
    let mut stmts = Vec::<Statement>::new();
    let mut scope_stack = ScopeStack::new();
    let mut errors = vec![];

    for parsed_stmt in parsed.stmts {
        let stmt_opt: Option<_> = match parsed_stmt {
            ParsedStmt::Normal(normal) => match normal {
                NormalStmt::Assert { mesg, cond } => {
                    let mut failure = false;
                    let mesg_ty = get_type(&mesg, &scope_stack);
                    if mesg_ty != Type::Str {
                        errors.push(Error("Expected Str".to_string(), mesg.span()));
                        failure = true;
                    }

                    let cond_ty = get_type(&cond, &scope_stack);
                    if cond_ty != Type::Bool {
                        errors.push(Error("Expected Bool".to_string(), cond.span()));
                        failure = true;
                    }

                    if failure {
                        None
                    } else {
                        Some(Statement::Assert { mesg, cond })
                    }
                }
                NormalStmt::Call { name } => {
                    let info = scope_stack.get_type_info(&LVal::Scalar(name.clone()));
                    if info.is_none() || info.unwrap().ty != Type::Sub {
                        errors.push(Error(
                            format!("Subroutine \"{}\" was not found", name),
                            Default::default(), // TODO: implement span retrieval
                        ));
                        None
                    } else {
                        Some(Statement::Call { name: name.clone() })
                    }
                }
                NormalStmt::Halt => Some(Statement::Halt),
                NormalStmt::Input { prompt, target } => {
                    let as_num_opt = if let Some(info) = scope_stack.get_type_info(&target) {
                        if !info.is_mut {
                            errors.push(Error(
                                format!("LVal \"{}\" is immutable", target),
                                Default::default(), // TODO: implement span retrieval
                            ));
                        }
                        Some(match info.ty {
                            Type::Num => true,
                            Type::Str => false,
                            // TODO: proper error handling
                            _ => die!("Expected Num or Str, found {}", info.ty),
                        })
                    } else {
                        errors.push(Error(
                            format!("LVal \"{}\" is invalid", target),
                            Default::default(), // TODO: implement span retrieval
                        ));
                        None
                    };

                    as_num_opt.map(|as_num| Statement::Input {
                        prompt,
                        target,
                        as_num,
                    })
                }
                NormalStmt::Let { name, init, is_mut } => {
                    let init_ty = get_type(&init, &scope_stack);
                    let success = scope_stack.add_var(
                        name.clone(),
                        TypeInfo {
                            ty: init_ty,
                            is_mut,
                        },
                    );

                    if success {
                        Some(Statement::Let {
                            name: name.clone(),
                            init,
                            is_mut,
                        })
                    } else {
                        errors.push(Error(
                            format!("Conflicting variable name \"{}\"", name),
                            Default::default(), // TODO: implement span retrieval
                        ));
                        None
                    }
                }
                NormalStmt::Modify { target, expr } => {
                    let lval_tinfo_opt = if let Some(info) = scope_stack.get_type_info(&target) {
                        if info.is_mut {
                            Some(info)
                        } else {
                            errors.push(Error(
                                format!("LVal \"{}\" is immutable", target),
                                Default::default(), // TODO: implement span retrieval
                            ));
                            None
                        }
                    } else {
                        errors.push(Error(
                            format!("LVal \"{}\" is invalid", target),
                            Default::default(), // TODO: implement span retrieval
                        ));
                        None
                    };

                    if let Some(lval_tinfo) = lval_tinfo_opt {
                        let expr_ty = get_type(&expr, &scope_stack);

                        if lval_tinfo.ty == expr_ty {
                            Some(Statement::Modify { target, expr })
                        } else {
                            errors.push(Error(
                                format!(
                                    "Type mismatch, expected {}, got {}",
                                    lval_tinfo.ty, expr_ty
                                ),
                                Default::default(), // TODO: implement span retrieval
                            ));
                            None
                        }
                    } else {
                        None
                    }
                }
                NormalStmt::Print { args } => {
                    let mut failure = false;
                    for a in &args {
                        let ty = get_type(a, &scope_stack);
                        if !ty.is_printable() {
                            errors.push(Error(
                                format!("Value of type {} cannot be printed", ty),
                                a.span(),
                            ));
                            failure = true;
                        }
                    }

                    if failure {
                        None
                    } else {
                        Some(Statement::Print { args: args.clone() })
                    }
                }
                NormalStmt::Roll {
                    count,
                    face,
                    target,
                } => {
                    let mut failure = false;
                    let count_ty = get_type(&count, &scope_stack);
                    if count_ty != Type::Num {
                        errors.push(Error(
                            format!("Expected Num, found {}", count_ty),
                            count.span(),
                        ));
                        failure = true;
                    }

                    let face_ty = get_type(&face, &scope_stack);
                    if face_ty != Type::Num {
                        errors.push(Error(
                            format!("Expected Num, found {}", face_ty),
                            face.span(),
                        ));
                        failure = true;
                    }

                    if let Some(info) = scope_stack.get_type_info(&target) {
                        if info.ty != Type::Num {
                            errors.push(Error(
                                format!("Expected Num, found {}", info.ty),
                                Default::default(), // TODO: implement span retrieval
                            ));
                            failure = true;
                        }
                        if !info.is_mut {
                            errors.push(Error(
                                format!("LVal \"{}\" is immutable", target),
                                Default::default(), // TODO: implement span retrieval
                            ));
                            failure = true;
                        }
                    } else {
                        errors.push(Error(
                            format!("LVal \"{}\" is invalid", target),
                            Default::default(), // TODO: implement span retrieval
                        ));
                        failure = true;
                    };

                    if failure {
                        None
                    } else {
                        Some(Statement::Roll {
                            count,
                            face,
                            target,
                        })
                    }
                }
            },
            ParsedStmt::Block(block, _) => match block {
                BlockStmt::For { counter, from, to } => {
                    // "For" <name> "from" <expr> "to" <expr> ";"
                    let mut failure = false;

                    let from_ty = get_type(&from, &scope_stack);
                    if from_ty != Type::Num {
                        errors.push(Error(
                            format!("Expected Num, found {}", from_ty),
                            from.span(),
                        ));
                        failure = true;
                    }

                    let to_ty = get_type(&to, &scope_stack);
                    if to_ty != Type::Num {
                        errors.push(Error(format!("Expected Num, found {}", to_ty), to.span()));
                        failure = true;
                    }

                    scope_stack.push(ScopeKind::Loop, stmts.len());

                    // always successful because we are pushing it to a new scope
                    let _ = scope_stack.add_var(
                        counter.clone(),
                        TypeInfo {
                            ty: Type::Num,
                            is_mut: false,
                        },
                    );

                    if failure {
                        None
                    } else {
                        Some(Statement::For {
                            counter,
                            from,
                            to,
                            offset_to_end: 0,
                        })
                    }
                }
                BlockStmt::While { cond } => {
                    scope_stack.push(ScopeKind::Loop, stmts.len());
                    let cond_ty = get_type(&cond, &scope_stack);
                    if Type::Bool == cond_ty {
                        Some(Statement::While {
                            cond,
                            offset_to_end: 0,
                        })
                    } else {
                        errors.push(Error(
                            format!("Expected Bool, found {}", cond_ty),
                            cond.span(),
                        ));
                        None
                    }
                }
                BlockStmt::Break => {
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

                    if loop_found {
                        Some(Statement::Break)
                    } else {
                        errors.push(Error(
                            "Break must be in a loop".to_string(),
                            Default::default(), // TODO: implement span retrieval
                        ));
                        None
                    }
                }
                BlockStmt::Continue => {
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

                    if loop_found {
                        Some(Statement::Continue)
                    } else {
                        errors.push(Error(
                            "Break must be in a loop".to_string(),
                            Default::default(), // TODO: implement span retrieval
                        ));
                        None
                    }
                }
                BlockStmt::If { cond } => {
                    scope_stack.push(ScopeKind::Other, stmts.len());

                    Some(Statement::If {
                        cond,
                        offset_to_next: 0,
                    })
                }
                BlockStmt::ElIf { cond } => {
                    let prev_idx = scope_stack.pop();

                    if let Some(prev_idx) = prev_idx {
                        let offset_to_next = stmts.len() - prev_idx;

                        let prev = stmts[prev_idx].clone();
                        stmts[prev_idx] = match prev {
                            Statement::If { cond, .. } => Statement::If {
                                cond: cond.clone(),
                                offset_to_next,
                            },
                            Statement::ElIf { cond, .. } => Statement::ElIf {
                                cond: cond.clone(),
                                offset_to_next,
                            },
                            _ => {
                                // TODO: proper error handle
                                panic!("Invalid statement was finding Else-If");
                            }
                        };

                        scope_stack.push(ScopeKind::Other, stmts.len());
                        Some(Statement::ElIf {
                            cond,
                            offset_to_next: 0,
                        })
                    } else {
                        errors.push(Error(
                            "A stray Else-If detected.".to_string(),
                            Default::default(), // TODO: implement span retrieval
                        ));
                        None
                    }
                }
                BlockStmt::Else => {
                    let prev_idx = scope_stack.pop();

                    if let Some(prev_idx) = prev_idx {
                        let offset_to_next = stmts.len() - prev_idx;

                        let prev = stmts[prev_idx].clone();
                        stmts[prev_idx] = match prev {
                            Statement::If { cond, .. } => Statement::If {
                                cond: cond.clone(),
                                offset_to_next,
                            },
                            Statement::ElIf { cond, .. } => Statement::ElIf {
                                cond: cond.clone(),
                                offset_to_next,
                            },
                            _ => {
                                // TODO: proper error handle
                                panic!("Invalid statement was finding Else");
                            }
                        };
                        scope_stack.push(ScopeKind::Other, stmts.len());
                        Some(Statement::Else { offset_to_end: 0 })
                    } else {
                        errors.push(Error(
                            "A stray Else detected.".to_string(),
                            Default::default(), // TODO: implement span retrieval
                        ));
                        None
                    }
                }
                BlockStmt::Sub { name } => {
                    // create new scope
                    scope_stack.push(ScopeKind::Sub, stmts.len());

                    // add this sub to var table
                    let success = scope_stack.add_var(
                        name.clone(),
                        TypeInfo {
                            ty: Type::Sub,
                            is_mut: false,
                        },
                    );

                    if success {
                        Some(Statement::Sub {
                            name: name.clone(),
                            offset_to_end: 0,
                        })
                    } else {
                        errors.push(Error(
                            format!("Conflicting subroutine name \"{}\"", name),
                            Default::default(), // TODO: implement span retrieval
                        ));
                        None
                    }
                }
                BlockStmt::Return => {
                    // "Return" ";"
                    let mut sub_found = false;
                    for k in scope_stack.kinds() {
                        if let ScopeKind::Sub = k {
                            sub_found = true;
                            break;
                        }
                    }

                    if sub_found {
                        Some(Statement::Return)
                    } else {
                        errors.push(Error(
                            "Return must be in a sub".to_string(),
                            Default::default(), // TODO: implement span retrieval
                        ));
                        None
                    }
                }
                BlockStmt::End => {
                    // Pop stack and assign end index
                    let prev_idx = scope_stack.pop();

                    if let Some(prev_idx) = prev_idx {
                        let offset_to_end = stmts.len() - prev_idx;

                        let prev = stmts[prev_idx].clone();
                        stmts[prev_idx] = match prev {
                            Statement::Sub { name, .. } => Statement::Sub {
                                name,
                                offset_to_end,
                            },
                            Statement::While { cond, .. } => Statement::While {
                                cond,
                                offset_to_end,
                            },
                            Statement::If { ref cond, .. } => Statement::If {
                                cond: cond.clone(),
                                offset_to_next: offset_to_end,
                            },
                            Statement::ElIf { ref cond, .. } => Statement::ElIf {
                                cond: cond.clone(),
                                offset_to_next: offset_to_end,
                            },
                            Statement::Else { .. } => Statement::Else { offset_to_end },
                            Statement::For {
                                counter, from, to, ..
                            } => Statement::For {
                                counter,
                                from,
                                to,
                                offset_to_end,
                            },
                            _ => {
                                // TODO: proper error handling
                                panic!("Invalid statement was finding End");
                            }
                        };

                        Some(Statement::End)
                    } else {
                        errors.push(Error(
                            "A stray End detected.".to_string(),
                            Default::default(), // TODO: implement span retrieval
                        ));
                        None
                    }
                }
            },
            ParsedStmt::Ill => Some(Statement::Ill),
        };

        if let Some(stmt) = stmt_opt {
            stmts.push(stmt);
        }
    }

    if !errors.is_empty() {
        Err(errors)
    } else if !scope_stack.is_empty() {
        // TODO: replace with proper handler when scopes have their starting stmt
        todo!("scope_stack is not empty")
    } else {
        Ok(Ast { stmts })
    }
}
