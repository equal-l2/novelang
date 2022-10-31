use crate::exprs::Expr;
use crate::parse::stmt::call::Call;
use crate::span::*;
use crate::target::Target;
use crate::types::IdentName;

mod exprs;
use exprs::TypeCheck;

mod stmt;
mod types;
pub use types::Type;

use crate::parse::stmt::sub::*;

#[derive(Debug, Clone)]
pub enum Statement {
    Assert {
        mesg: Expr,
        cond: Expr,
    },
    Call(Call),
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

    Break,
    Continue,
    Return(Option<Expr>),

    Halt,

    For(For),
    While(While),
    If(If),
    Sub(Sub),

    Ill,
}

#[derive(Debug, Clone)]
struct For {
    counter: IdentName,
    from: Expr,
    to: Expr,
    body: StmtList,
}

#[derive(Debug, Clone)]
struct While {
    cond: Expr,
    body: StmtList,
}

#[derive(Debug, Clone)]
struct If {
    branches: Vec<Conditional>,
    r#else: StmtList,
}

#[derive(Debug, Clone)]
struct Sub {
    name: SubName,
    args: Option<SubArgs>,
    res: Option<SubRes>,
    body: StmtList,
}

pub type StmtList = Vec<Statement>;

#[derive(Debug, Clone)]
pub struct Conditional {
    pub cond: Expr,
    pub body: StmtList,
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub stmts: StmtList,
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
    kind: ScopeKind,
}

#[derive(Debug)]
enum ScopeKind {
    Sub(Option<Type>),
    Loop,
    Normal,
}

impl Scope {
    fn new() -> Self {
        Self {
            map: VarMap::new(),
            kind: ScopeKind::Normal,
        }
    }

    fn new_special(kind: ScopeKind) -> Self {
        Self {
            map: VarMap::new(),
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

    fn is_sub(&self) -> bool {
        matches!(self.kind, ScopeKind::Sub(_))
    }

    fn is_loop(&self) -> bool {
        matches!(self.kind, ScopeKind::Loop)
    }

    fn res_ty(&self) -> Option<&Option<Type>> {
        if let ScopeKind::Sub(ty) = self.kind {
            Some(&ty)
        } else {
            None
        }
    }
}

trait ScopeStack {
    fn get_top_mut(&mut self) -> &mut Scope;

    fn add_var(&mut self, name: IdentName, var: TypeInfo) -> bool;

    fn get_type_info(&self, target: &Target) -> Result<TypeInfo, exprs::ErrorKind>;

    fn is_empty(&self) -> bool;

    fn res_ty(&self) -> Option<&Option<Type>>;
}

impl ScopeStack for &mut Vec<Scope> {
    fn get_top_mut(&mut self) -> &mut Scope {
        self.last_mut().unwrap()
    }

    fn add_var(&mut self, name: IdentName, var: TypeInfo) -> bool {
        self.get_top_mut().add_var(name, var)
    }

    fn get_type_info(&self, target: &Target) -> Result<TypeInfo, exprs::ErrorKind> {
        match target {
            Target::Scalar(i) => self
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
        self.len() == 1
    }

    fn res_ty(&self) -> Option<&Option<Type>> {
        self.iter()
            .rfind(|x| x.is_sub())
            .map(|sc| sc.res_ty().unwrap())
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
    ($expr: expr, $expected: expr, $scope_stack: ident, $errors: ident) => {{
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
    }};
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

trait CheckSemantics {
    fn check_semantics(&self, scopes: &mut Vec<Scope>) -> Result<(), Vec<(Error, Span)>>;
}

impl CheckSemantics for crate::block::BlockList {
    fn check_semantics(&self, scopes: &mut Vec<Scope>) -> Result<(), Vec<(Error, Span)>> {
        use crate::block::Statement;
        let mut errors = vec![];
        for block in self {
            match block {
                Statement::Normal(normal) => {
                    if let Err(errs) = normal.check_semantics(scopes) {
                        errors.extend(errs.into_iter());
                    }
                }
                Statement::Block(block) => {
                    if let Err(errs) = block.check_semantics(scopes) {
                        errors.extend(errs.into_iter());
                    }
                }
                Statement::Ill => { /* no-op */ }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}

impl CheckSemantics for crate::parse::NormalStmt {
    fn check_semantics(&self, scopes: &mut Vec<Scope>) -> Result<(), Vec<(Error, Span)>> {
        use crate::parse::{stmt::call::*, stmt::variant, NormalStmt};
        let mut errors = vec![];
        match self {
            NormalStmt::Assert(variant::Assert { mesg, cond }) => {
                check_expr_type!(mesg, Type::Str, scopes, errors);
                check_expr_type!(cond, Type::Bool, scopes, errors);
            }
            NormalStmt::Call(call) => {
                let callee_type = call.callee.0.check_type(&scopes);

                match callee_type {
                    Ok(Type::Sub {
                        args: args_expected,
                        res: res_actual,
                    }) => {
                        // check args type
                        match (args_expected, &call.args) {
                            (None, None) => { /* OK */ }
                            (Some(expected), Some(CallArgs(actual))) => {
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
                                        match arg.check_type(&scopes) {
                                            Ok(actual) => {
                                                if expected != actual {
                                                    errors.push((
                                                        Error::TypeMismatch { expected, actual },
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
                            (None, Some(CallArgs(actual))) => {
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
                        match (res_actual, &call.res) {
                            (None, None) => { /* OK */ }
                            (Some(expected), Some(CallRes(res))) => {
                                match scopes.get_type_info(res) {
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
                            (None, Some(CallRes(res))) => {
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
            }
            NormalStmt::Input(variant::Input { prompt, target }) => {
                match scopes.get_type_info(&target) {
                    Ok(TypeInfo { ty, is_mut }) => {
                        if !is_mut {
                            errors.push((
                                Error::Other(format!("Target \"{}\" is immutable", target)),
                                target.span(),
                            ));
                        }
                        match ty {
                            Type::Num | Type::Str => { /* OK */ }
                            _ => {
                                errors.push((
                                    Error::Other(format!("Expected Num or Str, found {}", ty)),
                                    target.span(),
                                ));
                            }
                        }
                    }
                    Err(e) => {
                        errors.push((Error::Expr(e), target.span()));
                    }
                }
            }
            NormalStmt::Let(variant::Let { name, init, is_mut }) => {
                let mut let_errors = vec![];

                let init_ty = match init.check_type(&scopes) {
                    Ok(ty) => ty,
                    Err(e) => {
                        let_errors.push((Error::Expr(e.kind), e.span));
                        Type::Invalid
                    }
                };
                let success = scopes.add_var(
                    name.0.clone(),
                    TypeInfo {
                        ty: init_ty,
                        is_mut: *is_mut,
                    },
                );

                if !success {
                    let_errors.push((Error::VariableAlreadyDefined(name.0.clone()), name.span()));
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
            }
            NormalStmt::Modify(variant::Modify { target, expr }) => {
                match scopes.get_type_info(&target) {
                    Ok(TypeInfo { ty, is_mut }) => {
                        if !is_mut {
                            errors.push((
                                Error::Other(format!("Target \"{}\" is immutable", target)),
                                target.span(),
                            ));
                        }
                        check_expr_type!(expr, ty, scopes, errors);
                    }
                    Err(e) => {
                        errors.push((Error::Expr(e), target.span()));
                    }
                };
            }
            NormalStmt::Print(variant::Print(args)) => {
                for a in args {
                    let ty = get_type!(a, scopes, errors);
                    if !ty.is_printable() {
                        errors.push((Error::NonPrintable(ty), a.span()));
                    }
                }
            }
            NormalStmt::Roll(variant::Roll {
                count,
                faces,
                target,
            }) => {
                check_expr_type!(count, Type::Num, scopes, errors);
                check_expr_type!(faces, Type::Num, scopes, errors);

                match scopes.get_type_info(&target) {
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
            }

            NormalStmt::Break => todo!("check if in loop"),
            NormalStmt::Continue => todo!("check if in loop"),

            NormalStmt::Return(crate::parse::stmt::r#return::Return(res)) => {
                // XXX: The following precondition is a lie
                if let Some(res_ty) = scopes.res_ty() {
                    match (res_ty, res) {
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
                        (Some(expected), Some(expr)) => match expr.check_type(&scopes) {
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
                } else {
                    todo!("return is not in a sub");
                }
            }

            NormalStmt::Halt => { /* no-op */ }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}

macro_rules! in_scope {
    ($scopes: ident, $new_scope: expr, $proc: block) => {
        $scopes.push($new_scope);
        $proc;
        $scopes.pop();
    };
}

impl CheckSemantics for crate::block::BlockStmt {
    fn check_semantics(&self, scopes: &mut Vec<Scope>) -> Result<(), Vec<(Error, Span)>> {
        let mut errors = vec![];
        use crate::block::*;
        match self {
            BlockStmt::For(For {
                counter,
                from,
                to,
                body,
            }) => {
                check_expr_type!(from, Type::Num, scopes, errors);
                check_expr_type!(to, Type::Num, scopes, errors);

                in_scope!(scopes, Scope::new_special(ScopeKind::Loop), {
                    // always successful because we are pushing it to a new scope
                    let _ = scopes.add_var(
                        counter.0.clone(),
                        TypeInfo {
                            ty: Type::Num,
                            is_mut: false,
                        },
                    );

                    if let Err(errs) = body.check_semantics(scopes) {
                        errors.extend(errs);
                    }
                });
            }
            BlockStmt::While(While { cond, body }) => {
                check_expr_type!(cond, Type::Bool, scopes, errors);
                in_scope!(scopes, Scope::new_special(ScopeKind::Loop), {
                    if let Err(errs) = body.check_semantics(scopes) {
                        errors.extend(errs);
                    }
                });
            }
            BlockStmt::If(If { branches, r#else }) => {
                for Conditional { cond, body } in branches {
                    check_expr_type!(cond, Type::Bool, scopes, errors);
                    in_scope!(scopes, Scope::new_special(ScopeKind::Loop), {
                        if let Err(errs) = body.check_semantics(scopes) {
                            errors.extend(errs);
                        }
                    });
                }

                if let Some(r#else) = r#else {
                    in_scope!(scopes, Scope::new_special(ScopeKind::Loop), {
                        if let Err(errs) = r#else.check_semantics(scopes) {
                            errors.extend(errs);
                        }
                    });
                }
            }
            BlockStmt::Sub(Sub {
                name,
                args,
                res,
                body,
            }) => {
                // Add this sub to var table BEFORE creating a new scope
                let success = scopes.add_var(
                    name.clone().0.into(),
                    TypeInfo {
                        ty: Type::Sub {
                            args: args
                                .as_ref()
                                .map(|SubArgs(v)| v.iter().map(|a| a.ty.clone().into()).collect()),
                            res: res.as_ref().map(|SubRes(ty)| Box::new(ty.clone().into())),
                        },
                        is_mut: false,
                    },
                );

                if !success {
                    errors.push((
                        Error::SubroutineAlreadyDefined(name.clone().0.into()),
                        name.0.span(),
                    ));
                }

                let res_ty = res.clone().map(|SubRes(ty)| ty.into());

                in_scope!(scopes, Scope::new_special(ScopeKind::Sub(res_ty)), {
                    // add args to vars
                    if let Some(SubArgs(ref args)) = args {
                        for SubArg { ident, ty } in args {
                            scopes
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

                    if let Err(errs) = body.check_semantics(scopes) {
                        errors.extend(errs);
                    }
                });
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}

pub fn check_semantics(parsed: crate::block::BlockParsed) -> Result<Ast, Vec<(Error, Span)>> {
    let internals = {
        const INTERNAL_VARS: &'static [(&'static str, Type)] =
            &[("_wait", Type::Bool), ("_explicit_assert", Type::Bool)];

        let mut internals = Scope::new();
        for var in INTERNAL_VARS {
            internals.add_var(
                IdentName::from(var.0),
                TypeInfo {
                    ty: var.1.clone(),
                    is_mut: true,
                },
            );
        }
        internals
    };

    let mut scopes = vec![internals];

    parsed.blocks.check_semantics(&mut scopes)?;

    // TODO: proper error handling
    assert!(scopes.is_empty());

    // TODO: convert blocks into runtime statements

    todo!()
}
