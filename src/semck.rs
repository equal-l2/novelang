use crate::die;
use crate::exprs::Expr;
use crate::lval::LVal;
use crate::span::*;
use crate::types::IdentName;

mod exprs;
mod types;

use types::Type;

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

#[derive(Debug)]
struct Scope {
    map: VarMap,
    ret_idx: usize,
}

impl Scope {
    fn new(ret_idx: usize) -> Self {
        Self {
            map: VarMap::new(),
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
    const INTERNAL_VARS: &'static [(&'static str, Type)] =
        &[("_wait", Type::Bool), ("_explicit_assert", Type::Bool)];

    fn new() -> Self {
        let mut internals = Scope::new(0);
        for var in Self::INTERNAL_VARS {
            internals.add_var(
                IdentName::from(var.0),
                TypeInfo {
                    ty: var.1.clone(),
                    is_mut: true,
                },
            );
        }
        Self {
            scopes: vec![internals],
        }
    }

    fn push(&mut self, ret_idx: usize) {
        self.scopes.push(Scope::new(ret_idx));
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
                    // TODO: fix panic with non-array indexing
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

pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

pub enum ErrorKind {
    VariableAlreadyDefined(IdentName),
    SubroutineNotFound(IdentName),
    SubroutineAlreadyDefined(IdentName),
    NonPrintable(Type),
    TypeMismatch {
        // Expected <some-type>
        expected: Type,
        actual: Type,
    },
    Expr(exprs::ErrorKind),
    Other(String),
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::VariableAlreadyDefined(name) => {
                write!(f, "Variable named {} is already defined", name)
            }
            ErrorKind::SubroutineNotFound(name) => {
                write!(f, "Subroutine named {} was not found", name)
            }
            ErrorKind::SubroutineAlreadyDefined(name) => {
                write!(f, "Subroutine named {} is already defined", name)
            }
            ErrorKind::NonPrintable(ty) => write!(f, "Type {} is not printable", ty),
            ErrorKind::TypeMismatch { expected, actual } => {
                write!(f, "Expected type {}, found {}", expected, actual)
            }
            ErrorKind::Expr(e) => write!(f, "{}", e),
            ErrorKind::Other(error) => write!(f, "{}", error),
        }
    }
}

macro_rules! check_expr_type {
    ($expr: ident, $expected: expr, $scope_stack: ident, $errors: ident) => {
        match $expr.check_type(&$scope_stack) {
            Ok(ty) if ty == $expected => { /* OK */ }
            Ok(ty) => {
                $errors.push(Error {
                    kind: ErrorKind::TypeMismatch {
                        expected: $expected,
                        actual: ty,
                    },
                    span: $expr.span(),
                });
            }
            Err(e) => {
                $errors.push(Error {
                    kind: ErrorKind::Expr(e.kind),
                    span: e.span,
                });
            }
        }
    };
}

macro_rules! get_type {
    ($expr: ident, $scope_stack: ident, $errors: ident) => {
        match $expr.check_type(&$scope_stack) {
            Ok(ty) => ty,
            Err(e) => {
                $errors.push(Error {
                    kind: ErrorKind::Expr(e.kind),
                    span: e.span,
                });
                Type::Invalid
            }
        }
    };
}

pub fn check_semantics(parsed: crate::block::BlockChecked) -> Result<Ast, Vec<Error>> {
    use crate::block::{BlockStmt, Statement as ParsedStmt};
    use crate::parse::NormalStmt;
    use exprs::TypeCheck;
    let mut stmts = Vec::<Statement>::new();
    let mut scope_stack = ScopeStack::new();
    let mut errors = vec![];

    for parsed_stmt in parsed.stmts {
        stmts.push(match parsed_stmt {
            ParsedStmt::Normal(normal) => match normal {
                NormalStmt::Assert { mesg, cond } => {
                    check_expr_type!(mesg, Type::Str, scope_stack, errors);
                    check_expr_type!(cond, Type::Bool, scope_stack, errors);
                    Statement::Assert { mesg, cond }
                }
                NormalStmt::Call { name } => {
                    let info = scope_stack.get_type_info(&LVal::Scalar(name.clone()));
                    if info.is_none() || info.unwrap().ty != Type::Sub {
                        errors.push(Error {
                            kind: ErrorKind::SubroutineNotFound(name.clone()),
                            span: Default::default(), // TODO: implement span retrieval
                        });
                    }
                    Statement::Call { name: name.clone() }
                }
                NormalStmt::Halt => Statement::Halt,
                NormalStmt::Input { prompt, target } => {
                    let as_num = if let Some(info) = scope_stack.get_type_info(&target) {
                        if !info.is_mut {
                            errors.push(Error {
                                kind: ErrorKind::Other(format!("LVal \"{}\" is immutable", target)),
                                span: Default::default(), // TODO: implement span retrieval
                            });
                        }
                        match info.ty {
                            Type::Num => true,
                            Type::Str => false,
                            // TODO: proper error handling
                            _ => die!("Expected Num or Str, found {}", info.ty),
                        }
                    } else {
                        errors.push(Error {
                            kind: ErrorKind::Other(format!("LVal \"{}\" is invalid", target)),
                            span: Default::default(), // TODO: implement span retrieval
                        });
                        false
                    };

                    Statement::Input {
                        prompt,
                        target,
                        as_num,
                    }
                }
                NormalStmt::Let { name, init, is_mut } => {
                    let init_ty = get_type!(init, scope_stack, errors);
                    let success = scope_stack.add_var(
                        name.clone(),
                        TypeInfo {
                            ty: init_ty,
                            is_mut,
                        },
                    );

                    if !success {
                        errors.push(Error {
                            kind: ErrorKind::VariableAlreadyDefined(name.clone()),
                            span: Default::default(), // TODO: implement span retrieval
                        });
                    }
                    Statement::Let {
                        name: name.clone(),
                        init,
                        is_mut,
                    }
                }
                NormalStmt::Modify { target, expr } => {
                    if let Some(info) = scope_stack.get_type_info(&target) {
                        if !info.is_mut {
                            errors.push(Error {
                                kind: ErrorKind::Other(format!("LVal \"{}\" is immutable", target)),
                                span: Default::default(), // TODO: implement span retrieval
                            });
                        }
                        check_expr_type!(expr, info.ty, scope_stack, errors);
                    } else {
                        errors.push(Error {
                            kind: ErrorKind::Other(format!("LVal \"{}\" is invalid", target)),
                            span: Default::default(), // TODO: implement span retrieval
                        });
                    }

                    Statement::Modify { target, expr }
                }
                NormalStmt::Print { args } => {
                    for a in &args {
                        let ty = get_type!(a, scope_stack, errors);
                        if !ty.is_printable() {
                            errors.push(Error {
                                kind: ErrorKind::NonPrintable(ty),
                                span: a.span(),
                            });
                        }
                    }

                    Statement::Print { args: args.clone() }
                }
                NormalStmt::Roll {
                    count,
                    face,
                    target,
                } => {
                    check_expr_type!(count, Type::Num, scope_stack, errors);
                    check_expr_type!(face, Type::Num, scope_stack, errors);

                    if let Some(info) = scope_stack.get_type_info(&target) {
                        if info.ty != Type::Num {
                            errors.push(Error {
                                kind: ErrorKind::TypeMismatch {
                                    expected: Type::Num,
                                    actual: info.ty,
                                },
                                span: Default::default(), // TODO: implement span retrieval
                            });
                        }
                        if !info.is_mut {
                            errors.push(Error {
                                kind: ErrorKind::Other(format!("LVal \"{}\" is immutable", target)),
                                span: Default::default(), // TODO: implement span retrieval
                            });
                        }
                    } else {
                        errors.push(Error {
                            kind: ErrorKind::Other(format!("LVal \"{}\" is invalid", target)),
                            span: Default::default(), // TODO: implement span retrieval
                        });
                    };

                    Statement::Roll {
                        count,
                        face,
                        target,
                    }
                }
            },
            ParsedStmt::Block(block) => match block {
                BlockStmt::For {
                    counter,
                    from,
                    to,
                    offset_to_end,
                } => {
                    // "For" <name> "from" <expr> "to" <expr> ";"
                    check_expr_type!(from, Type::Num, scope_stack, errors);
                    check_expr_type!(to, Type::Num, scope_stack, errors);

                    scope_stack.push(stmts.len());

                    // always successful because we are pushing it to a new scope
                    let _ = scope_stack.add_var(
                        counter.clone(),
                        TypeInfo {
                            ty: Type::Num,
                            is_mut: false,
                        },
                    );

                    Statement::For {
                        counter,
                        from,
                        to,
                        offset_to_end,
                    }
                }
                BlockStmt::While {
                    cond,
                    offset_to_end,
                } => {
                    scope_stack.push(stmts.len());
                    check_expr_type!(cond, Type::Bool, scope_stack, errors);
                    Statement::While {
                        cond,
                        offset_to_end,
                    }
                }
                BlockStmt::Break => Statement::Break,
                BlockStmt::Continue => {
                    // "Continue" ";"
                    Statement::Continue
                }
                BlockStmt::If {
                    cond,
                    offset_to_next,
                } => {
                    scope_stack.push(stmts.len());

                    check_expr_type!(cond, Type::Bool, scope_stack, errors);

                    Statement::If {
                        cond,
                        offset_to_next,
                    }
                }
                BlockStmt::ElIf {
                    cond,
                    offset_to_next,
                } => {
                    let _ = scope_stack.pop();

                    scope_stack.push(stmts.len());

                    check_expr_type!(cond, Type::Bool, scope_stack, errors);

                    Statement::ElIf {
                        cond,
                        offset_to_next,
                    }
                }
                BlockStmt::Else { offset_to_end } => {
                    let _ = scope_stack.pop();

                    scope_stack.push(stmts.len());
                    Statement::Else { offset_to_end }
                }
                BlockStmt::Sub {
                    name,
                    offset_to_end,
                } => {
                    // Add this sub to var table BEFORE creating a new scope
                    let success = scope_stack.add_var(
                        name.clone(),
                        TypeInfo {
                            ty: Type::Sub,
                            is_mut: false,
                        },
                    );

                    if !success {
                        errors.push(Error {
                            kind: ErrorKind::SubroutineAlreadyDefined(name.clone()),
                            span: Default::default(), // TODO: implement span retrieval
                        });
                    }

                    // create new scope
                    scope_stack.push(stmts.len());

                    Statement::Sub {
                        name: name.clone(),
                        offset_to_end,
                    }
                }
                BlockStmt::Return => {
                    // "Return" ";"
                    Statement::Return
                }
                BlockStmt::End => {
                    // Pop stack and assign end index
                    let _ = scope_stack.pop();

                    Statement::End
                }
            },
            ParsedStmt::Ill => Statement::Ill,
        });
    }

    debug_assert!(scope_stack.is_empty());

    if errors.is_empty() {
        Ok(Ast { stmts })
    } else {
        Err(errors)
    }
}
