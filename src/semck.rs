use crate::exprs::Expr;
use crate::parse::Call;
use crate::span::*;
use crate::target::Target;
use crate::types::IdentName;

mod exprs;
mod stmt;
mod types;
pub use stmt::sub::{Arg, Sub};

pub use types::Type;

#[derive(Debug, Clone)]
pub enum Statement {
    Assert {
        mesg: Expr,
        cond: Expr,
    },
    Call(Call),
    Halt,
    Input {
        prompt: Option<String>,
        target: Target,
        as_num: bool,
    },
    Let {
        name: IdentName,
        init: Expr,
        is_mut: bool,
    },
    Modify {
        target: Target,
        expr: Expr,
    },
    Print {
        args: Vec<Expr>,
    },
    Roll {
        count: Expr,
        faces: Expr,
        target: Target,
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
        sub: Sub,
        offset_to_end: usize,
    },
    Return(Option<Expr>),
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
    kind: ScopeKind,
}

#[derive(Debug)]
enum ScopeKind {
    Sub(Option<Type>),
    Other,
}

impl Scope {
    fn new(ret_idx: usize, kind: ScopeKind) -> Self {
        Self {
            map: VarMap::new(),
            ret_idx,
            kind,
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

#[derive(Debug)]
struct ScopeStack {
    scopes: Vec<Scope>,
}

impl ScopeStack {
    const INTERNAL_VARS: &'static [(&'static str, Type)] =
        &[("_wait", Type::Bool), ("_explicit_assert", Type::Bool)];

    fn new() -> Self {
        let mut internals = Scope::new(0, ScopeKind::Other);
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
        self.scopes.push(Scope::new(ret_idx, ScopeKind::Other));
    }

    fn push_with_res(&mut self, ret_idx: usize, res_ty: Option<Type>) {
        self.scopes
            .push(Scope::new(ret_idx, ScopeKind::Sub(res_ty)));
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

    fn get_type_info(&self, target: &Target) -> Result<TypeInfo, exprs::ErrorKind> {
        match target {
            Target::Scalar(i) => self
                .scopes
                .iter()
                .rev()
                .map(|m| m.get_type_info(&i.0))
                .find(Option::is_some)
                .flatten()
                .cloned()
                .ok_or_else(|| exprs::ErrorKind::VariableNotFound(i.0.clone())),
            Target::Vector(l, _) => {
                let v = self.get_type_info(l);
                match v {
                    Ok(TypeInfo { ty, is_mut }) => match ty {
                        Type::Arr(i) => Ok(TypeInfo {
                            ty: (*i).clone(),
                            is_mut,
                        }),
                        _ => Err(exprs::ErrorKind::NotIndexable(ty)),
                    },
                    Err(e) => Err(e),
                }
            }
        }
    }

    fn is_empty(&self) -> bool {
        self.scopes.len() == 1
    }

    fn res_ty(&self) -> Option<&Option<Type>> {
        self.scopes
            .iter()
            .rfind(|x| matches!(x.kind, ScopeKind::Sub(_)))
            .map(|sc| {
                if let ScopeKind::Sub(ref ty) = sc.kind {
                    ty
                } else {
                    unreachable!()
                }
            })
    }
}

pub enum Error {
    VariableAlreadyDefined(IdentName),
    SubroutineNotFound(IdentName),
    SubroutineAlreadyDefined(IdentName),
    NonPrintable(Type),
    TypeMismatch {
        // Expected <some-type>
        expected: Type,
        actual: Type,
    },
    ArgsCountMismatch {
        // Expected <some-type>
        expected: usize,
        actual: usize,
    },
    Expr(exprs::ErrorKind),
    GodMistake(Box<Self>),
    Other(String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::VariableAlreadyDefined(name) => {
                write!(f, "Variable named \"{}\" is already defined", name)
            }
            Error::SubroutineNotFound(name) => {
                write!(f, "Subroutine named \"{}\" was not found", name)
            }
            Error::SubroutineAlreadyDefined(name) => {
                write!(f, "Subroutine named \"{}\" is already defined", name)
            }
            Error::NonPrintable(ty) => write!(f, "Type \"{}\" is not printable", ty),
            Error::TypeMismatch { expected, actual } => {
                write!(f, "Expected type \"{}\", found \"{}\"", expected, actual)
            }
            Error::ArgsCountMismatch { expected, actual } => {
                write!(f, "Expected {} arg(s), found {}", expected, actual)
            }
            Error::Expr(e) => write!(f, "{}", e),
            Error::GodMistake(e) => write!(f, "Can God make a mistake? ({})", e),
            Error::Other(error) => write!(f, "{}", error),
        }
    }
}

macro_rules! check_expr_type {
    ($expr: expr, $expected: expr, $scope_stack: ident, $errors: ident) => {
        match $expr.check_type(&$scope_stack) {
            Ok(ty) if ty == $expected => { /* OK */ }
            Ok(ty) => {
                $errors.push((
                    Error::TypeMismatch {
                        expected: $expected,
                        actual: ty,
                    },
                    $expr.span(),
                ));
            }
            Err(e) => {
                $errors.push((Error::Expr(e.kind), e.span));
            }
        }
    };
}

macro_rules! get_type {
    ($expr: ident, $scope_stack: ident, $errors: ident) => {
        match $expr.check_type(&$scope_stack) {
            Ok(ty) => ty,
            Err(e) => {
                $errors.push((Error::Expr(e.kind), e.span));
                Type::Invalid
            }
        }
    };
}

pub fn check_semantics(parsed: crate::block::BlockChecked) -> Result<Ast, Vec<(Error, Span)>> {
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
                NormalStmt::Call(call) => {
                    let callee_type = call.callee.check_type(&scope_stack);

                    match callee_type {
                        Ok(Type::Sub { args, res }) => {
                            // check args type
                            match (args, &call.args) {
                                (None, None) => { /* OK */ }
                                (Some(expected), Some(actual)) => {
                                    let actual_span = Span(
                                        actual.first().unwrap().span().0,
                                        actual.last().unwrap().span().1,
                                    );
                                    if expected.len() != actual.len() {
                                        errors.push((
                                            Error::ArgsCountMismatch {
                                                expected: expected.len(),
                                                actual: actual.len(),
                                            },
                                            actual_span,
                                        ));
                                    } else {
                                        for (expected, arg) in std::iter::zip(expected, actual) {
                                            match arg.check_type(&scope_stack) {
                                                Ok(actual) => {
                                                    if expected != actual {
                                                        errors.push((
                                                            Error::TypeMismatch {
                                                                expected,
                                                                actual,
                                                            },
                                                            arg.span(),
                                                        ));
                                                    }
                                                }
                                                Err(e) => {
                                                    errors.push((Error::Expr(e.kind), e.span));
                                                }
                                            }
                                        }
                                    }
                                }
                                (Some(expected), None) => {
                                    errors.push((
                                        Error::ArgsCountMismatch {
                                            expected: expected.len(),
                                            actual: 0,
                                        },
                                        // TODO: correct span (whole statement?)
                                        call.callee.span(),
                                    ));
                                }
                                (None, Some(actual)) => {
                                    errors.push((
                                        Error::ArgsCountMismatch {
                                            expected: 0,
                                            actual: actual.len(),
                                        },
                                        // span for actual args
                                        Span(
                                            actual.first().unwrap().span().0,
                                            actual.last().unwrap().span().1,
                                        ),
                                    ));
                                }
                            }

                            // check res type
                            match (res, &call.res) {
                                (None, None) => { /* OK */ }
                                (Some(expected), Some(res)) => {
                                    match scope_stack.get_type_info(res) {
                                        Ok(TypeInfo { ty: actual, is_mut }) => {
                                            if !is_mut {
                                                errors.push((
                                                    Error::Other("target is immutable".into()),
                                                    res.span(),
                                                ));
                                            } else if *expected != actual {
                                                errors.push((
                                                    Error::TypeMismatch {
                                                        expected: *expected,
                                                        actual,
                                                    },
                                                    res.span(),
                                                ));
                                            }
                                        }
                                        Err(e) => {
                                            errors.push((Error::Expr(e), res.span()));
                                        }
                                    }
                                }
                                (Some(_), None) => { /* Discard the result */ }
                                (None, Some(res)) => {
                                    errors.push((
                                        Error::Other("The called Sub returns no result".into()),
                                        res.span(),
                                    ));
                                }
                            }
                        }
                        Ok(ty) => {
                            errors.push((
                                Error::Other(format!("Expected Sub types, found {}", ty)),
                                call.callee.span(),
                            ));
                        }
                        Err(e) => {
                            errors.push((Error::Expr(e.kind), e.span));
                        }
                    }
                    Statement::Call(call.clone())
                }
                NormalStmt::Halt => Statement::Halt,
                NormalStmt::Input { prompt, target } => {
                    let as_num = match scope_stack.get_type_info(&target) {
                        Ok(TypeInfo { ty, is_mut }) => {
                            if !is_mut {
                                errors.push((
                                    Error::Other(format!("Target \"{}\" is immutable", target)),
                                    target.span(),
                                ));
                            }
                            match ty {
                                Type::Num => true,
                                Type::Str => false,
                                _ => {
                                    errors.push((
                                        Error::Other(format!("Expected Num or Str, found {}", ty)),
                                        target.span(),
                                    ));
                                    false
                                }
                            }
                        }
                        Err(e) => {
                            errors.push((Error::Expr(e), target.span()));
                            false
                        }
                    };

                    Statement::Input {
                        prompt,
                        target,
                        as_num,
                    }
                }
                NormalStmt::Let { name, init, is_mut } => {
                    let mut let_errors = vec![];

                    let init_ty = match init.check_type(&scope_stack) {
                        Ok(ty) => ty,
                        Err(e) => {
                            let_errors.push((Error::Expr(e.kind), e.span));
                            Type::Invalid
                        }
                    };
                    let success = scope_stack.add_var(
                        name.0.clone(),
                        TypeInfo {
                            ty: init_ty,
                            is_mut,
                        },
                    );

                    if !success {
                        let_errors
                            .push((Error::VariableAlreadyDefined(name.0.clone()), name.span()));
                    }

                    if name.as_ref() == "there" && init.to_string() == "light" && !is_mut {
                        errors.extend(
                            let_errors
                                .into_iter()
                                .map(|(e, s)| (Error::GodMistake(Box::new(e)), s)),
                        );
                    } else {
                        errors.extend(let_errors.into_iter());
                    }

                    Statement::Let {
                        name: name.0.clone(),
                        init,
                        is_mut,
                    }
                }
                NormalStmt::Modify { target, expr } => {
                    match scope_stack.get_type_info(&target) {
                        Ok(TypeInfo { ty, is_mut }) => {
                            if !is_mut {
                                errors.push((
                                    Error::Other(format!("Target \"{}\" is immutable", target)),
                                    target.span(),
                                ));
                            }
                            check_expr_type!(expr, ty, scope_stack, errors);
                        }
                        Err(e) => {
                            errors.push((Error::Expr(e), target.span()));
                        }
                    };

                    Statement::Modify { target, expr }
                }
                NormalStmt::Print { args } => {
                    for a in &args {
                        let ty = get_type!(a, scope_stack, errors);
                        if !ty.is_printable() {
                            errors.push((Error::NonPrintable(ty), a.span()));
                        }
                    }

                    Statement::Print { args: args.clone() }
                }
                NormalStmt::Roll {
                    count,
                    faces,
                    target,
                } => {
                    check_expr_type!(count, Type::Num, scope_stack, errors);
                    check_expr_type!(faces, Type::Num, scope_stack, errors);

                    match scope_stack.get_type_info(&target) {
                        Ok(TypeInfo { ty, is_mut }) => {
                            if ty != Type::Num {
                                errors.push((
                                    Error::TypeMismatch {
                                        expected: Type::Num,
                                        actual: ty,
                                    },
                                    target.span(),
                                ));
                            }
                            if !is_mut {
                                errors.push((
                                    Error::Other(format!("Target \"{}\" is immutable", target)),
                                    target.span(),
                                ));
                            }
                        }
                        Err(e) => {
                            errors.push((Error::Expr(e), target.span()));
                        }
                    };

                    Statement::Roll {
                        count,
                        faces,
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
                        counter.0.clone(),
                        TypeInfo {
                            ty: Type::Num,
                            is_mut: false,
                        },
                    );

                    Statement::For {
                        counter: counter.0,
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
                BlockStmt::Sub { sub, offset_to_end } => {
                    let name = &sub.name;

                    // Add this sub to var table BEFORE creating a new scope
                    let success = scope_stack.add_var(
                        name.clone().into(),
                        TypeInfo {
                            ty: Type::Sub {
                                args: sub
                                    .args
                                    .as_ref()
                                    .map(|v| v.iter().map(|a| a.ty.clone().into()).collect()),
                                res: sub.res.as_ref().map(|ty| Box::new(ty.clone().into())),
                            },
                            is_mut: false,
                        },
                    );

                    if !success {
                        errors.push((
                            Error::SubroutineAlreadyDefined(name.clone().into()),
                            name.span(),
                        ));
                    }

                    // create new scope
                    scope_stack.push_with_res(stmts.len(), sub.res.clone().map(Into::into));

                    // add args to vars
                    if let Some(ref args) = sub.args {
                        for crate::parse::Arg { ident, ty } in args {
                            scope_stack
                                .add_var(
                                    ident.as_ref().into(),
                                    TypeInfo {
                                        ty: ty.clone().into(),
                                        is_mut: false,
                                    },
                                )
                                .then_some(())
                                .unwrap();
                        }
                    }

                    Statement::Sub {
                        sub: sub.into(),
                        offset_to_end,
                    }
                }
                BlockStmt::Return(res) => {
                    // this should not panic because it is already ensured
                    // the return is in a sub scope
                    let res_ty = scope_stack.res_ty().unwrap();

                    match (res_ty, &res) {
                        (None, None) => { /* OK */ }
                        (Some(expected), None) => {
                            // TODO: needs a span pointing to the whole statement
                            todo!("expected a return value in type {}", expected);
                        }
                        (None, Some(expr)) => {
                            errors.push((
                                Error::Other("expected no return value".into()),
                                expr.span(),
                            ));
                        }
                        (Some(expected), Some(expr)) => match expr.check_type(&scope_stack) {
                            Ok(actual) => {
                                if expected != &actual {
                                    errors.push((
                                        Error::TypeMismatch {
                                            expected: expected.clone(),
                                            actual,
                                        },
                                        expr.span(),
                                    ));
                                }
                            }
                            Err(e) => {
                                errors.push((Error::Expr(e.kind), e.span));
                            }
                        },
                    }
                    Statement::Return(res)
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
