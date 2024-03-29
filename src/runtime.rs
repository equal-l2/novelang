mod exprs;
mod val;

use crate::die;
use crate::exprs::Expr;
use crate::parse::stmt::call::Call;
use crate::semck::{Ast, Statement};
use crate::target::Target;
use crate::types::{IdentName, IntType};
use once_cell::sync::Lazy;

use exprs::Eval;
use val::Val;

use crate::parse::stmt::call::{CallArgs, CallRes};

type VarTable = std::collections::HashMap<IdentName, Val>;

/// Represents a scope
struct Scope {
    kind: ScopeKind,
    ret_idx: usize,
    vars: VarTable,
}

impl Scope {
    fn new(kind: ScopeKind, ret_idx: usize) -> Self {
        Self {
            kind,
            ret_idx,
            vars: VarTable::new(),
        }
    }
}

#[derive(Debug)]
enum ScopeKind {
    Branch,
    Loop,
    Sub(Option<CallRes>),
    ForWrap,
}

/// Represents the store for runtime state
pub struct Runtime {
    stack: Vec<Scope>,
    globals: VarTable,
    internals: VarTable,
    rng: rand::rngs::SmallRng,
}

impl exprs::VarsMap for Runtime {
    fn get(&self, name: &IdentName) -> Val {
        self.get_var(name).clone()
    }

    fn get_arr_elem<L: Eval, R: Eval>(&self, l: &L, r: &R) -> exprs::Result {
        use exprs::EvalError;
        let r = r.eval(self)?;
        if let Val::Num(n) = r {
            let l = l.eval(self)?;
            match l {
                Val::Str(s) => s.chars().nth(n as usize).map(|c| Val::Str(c.into())),
                // TODO: check type with debug_assert?
                Val::Arr(_, v) => v.get(n as usize).cloned(),
                _ => unreachable!("Type {} cannot be indexed", l.typename()),
            }
            .ok_or(EvalError::IndexOutOfBounds(n))
        } else {
            unreachable!("non-Num index is not allowed");
        }
    }
}

static WAIT: Lazy<IdentName> = Lazy::new(|| "_wait".into());

impl Runtime {
    fn new() -> Self {
        use rand::SeedableRng;
        // internal variables
        // - "_wait": whether wait is enabled

        let internals = {
            let mut vt = VarTable::new();
            vt.insert(WAIT.clone(), Val::Bool(false));
            vt
        };

        let rng = rand::rngs::SmallRng::from_entropy();

        Self {
            stack: vec![],
            globals: VarTable::new(),
            internals,
            rng,
        }
    }

    /// Declare a variable
    /// Aborts when the variable is already declared in the scope
    fn decl_var(&mut self, name: IdentName, val: Val) {
        let var_table = if self.stack.is_empty() {
            &mut self.globals
        } else {
            &mut self.stack.last_mut().unwrap().vars
        };
        if var_table.insert(name.clone(), val).is_some() {
            die!("Runtime error: variable {} is already declared", name);
        }
    }

    fn resolve_target(&mut self, target: &Target) -> Result<&mut Val, exprs::EvalError> {
        match target {
            Target::Scalar(i) => Ok(self.get_var_mut(&i.0)),
            Target::Vector(l, r) => {
                let r = r.eval(self)?;
                if let Val::Num(n) = r {
                    let resolved = self.resolve_target(l)?;
                    if let Val::Arr(_, v) = resolved {
                        v.get_mut(n as usize)
                            .ok_or(exprs::EvalError::IndexOutOfBounds(n))
                    } else {
                        panic!("tried to index non-array var")
                    }
                } else {
                    unreachable!("non-Num index is not allowed");
                }
            }
        }
    }

    /// Modify a variable
    /// Aborts on error (the variable doesn't exists, differ in type, or is immutable)
    fn modify_var(&mut self, target: &Target, val: Val) {
        let var = self
            .resolve_target(target)
            .unwrap_or_else(|e| die!("Runtime error: cannot resolve {} because of: {}", target, e));
        *var = val;
    }

    /// Pop the current scope
    fn pop(&mut self) -> Option<Scope> {
        self.stack.pop()
    }

    fn peek(&self) -> Option<&Scope> {
        self.stack.last()
    }

    /// Push a new scope
    fn push(&mut self, kind: ScopeKind, ret_idx: usize) {
        self.stack.push(Scope::new(kind, ret_idx));
    }

    fn vars_iter(&self) -> impl Iterator<Item = &VarTable> {
        use std::iter::once;
        once(&self.internals)
            .chain(self.stack.iter().map(|f| &f.vars).rev())
            .chain(once(&self.globals))
    }

    fn vars_iter_mut(&mut self) -> impl Iterator<Item = &mut VarTable> {
        use std::iter::once;
        once(&mut self.internals)
            .chain(self.stack.iter_mut().map(|f| &mut f.vars).rev())
            .chain(once(&mut self.globals))
    }

    // get the highest variable in the stack with the specified name
    pub fn get_var(&self, name: &IdentName) -> &Val {
        self.vars_iter()
            .map(|t| t.get(name))
            .find(Option::is_some)
            .flatten()
            .expect("Variable must exist")
    }

    fn get_var_mut(&mut self, name: &IdentName) -> &mut Val {
        self.vars_iter_mut()
            .map(|t| t.get_mut(name))
            .find(Option::is_some)
            .flatten()
            .expect("Variable must exist")
    }

    pub fn exec_roll(&mut self, cnt: &Expr, faces: &Expr, name: &Target) {
        use rand::Rng;

        let cnt = unwrap_num(&cnt.eval(self).unwrap_or_else(|e| {
            die!("Runtime error: Failed to eval count of Roll: {}", e);
        }));
        let faces = unwrap_num(&faces.eval(self).unwrap_or_else(|e| {
            die!("Runtime error: Failed to eval faces of Roll: {}", e);
        }));

        if cnt <= 0 {
            die!("Runtime error: Count for Roll must be a positive integer");
        }

        if faces <= 0 {
            die!("Runtime error: Faces for Roll must be a positive integer");
        }

        let val = std::iter::repeat_with(|| self.rng.gen_range(1..=faces))
            .take(cnt as usize)
            .sum();

        self.modify_var(name, Val::Num(val));
    }

    pub fn exec_print(&mut self, args: &[Expr]) {
        let fun = || {
            use std::io::Write;
            let stdout = std::io::stdout();
            let mut lock = stdout.lock();

            if !args.is_empty() {
                let val = args[0].eval(self).unwrap_or_else(|e| {
                    die!("Runtime error: Failed to eval arg of Print: {:?}", e);
                });
                write!(lock, "{}", val)?;
            }
            for arg in &args[1..] {
                let val = arg.eval(self).unwrap_or_else(|e| {
                    die!("Runtime error: Failed to eval arg of Print: {:?}", e);
                });
                write!(lock, " {}", val)?;
            }
            writeln!(lock)?;
            lock.flush()?;

            let wait = unwrap_bool(self.get_var(&WAIT));

            if wait {
                write!(lock, "[Proceed with Enter⏎ ]")?;
                lock.flush()?;
                let _ = read_line_from_stdin();
                // move to the prev line and erase the line
                write!(lock, "\x1B[F\x1B[2K")?;
                lock.flush()?;
            }
            Ok(())
        };

        if let std::io::Result::Err(e) = fun() {
            die!("Error in print: {}", e);
        }
    }

    pub fn exec_input(&mut self, prompt: &Option<String>, name: &Target, as_num: bool) {
        if as_num {
            let input = get_input(prompt.as_deref());
            self.modify_var(name, Val::Num(input));
        } else {
            let input = get_input(prompt.as_deref());
            self.modify_var(name, Val::Str(input));
        }
    }

    pub fn exec_call(&mut self, Call { callee, args, res }: &Call, index: usize) -> usize {
        let var_sub = callee.0.eval(self).unwrap_or_else(|e| {
            // FIXME: better error handle
            die!("Runtime error: failed to eval Sub to call: {}", e);
        });

        if let Val::Sub(ref sub) = var_sub {
            // Check the args types
            match (sub.args.as_ref(), args) {
                (None, None) => { /* OK */ }
                (Some(expected), Some(CallArgs(actual))) => {
                    if expected.len() != actual.len() {
                        unreachable!("args count mismatch (semck defect)")
                    }

                    for (arg, expr) in std::iter::zip(expected.iter(), actual.iter()) {
                        let expected = &arg.ty;
                        let actual = expr.eval(self).unwrap().ty(); // FIXME: error
                        if expected != &actual {
                            unreachable!("args type mismatch (semck defect)")
                        }
                    }
                }
                _ => {
                    unreachable!("args existence mismatch (semck defect)")
                }
            }

            // Check the target type
            match (&sub.res_type, &res) {
                (None, None) => { /* OK */ }
                (Some(ret_expected), Some(CallRes(target))) => {
                    let ret_actual = self.resolve_target(target).unwrap(); // FIXME: error
                    if ret_expected != &ret_actual.ty() {
                        unreachable!("ret type mismatch (semck defect)")
                    }
                }
                (Some(_), None) => { /* Discard the return value */ }
                (None, Some(_)) => {
                    unreachable!("ret existence mismatch (semck defect)")
                }
            }

            // Push scope
            // register address to return (the next line)
            self.push(ScopeKind::Sub(res.clone().map(Into::into)), index + 1);

            // Add args
            if let Some(CallArgs(args)) = args {
                for (an_arg, expr) in std::iter::zip(sub.args.as_ref().unwrap(), args) {
                    let val = expr.eval(self).unwrap(); // TODO: error check
                    self.decl_var(an_arg.ident.clone(), val);
                }
            }

            sub.start
        } else {
            // FIXME: better error handle
            unreachable!("Runtime error: Sub expected, got {}", var_sub.typename());
        }
    }
}

fn read_line_from_stdin() -> String {
    use std::io::BufRead;
    let stdin = std::io::stdin();
    let mut it = stdin.lock().lines();
    it.next().unwrap_or_else(|| Ok("".to_owned())).unwrap()
}

fn get_input<T: std::str::FromStr>(prompt: Option<&str>) -> T {
    use std::io::Write;
    let stdout = std::io::stdout();
    let mut lock = stdout.lock();
    loop {
        write!(lock, "{} > ", prompt.unwrap_or("Provide an integer")).unwrap();
        let _ = lock.flush();
        if let Ok(i) = read_line_from_stdin().parse() {
            return i;
        }
        writeln!(lock, "!! Provided input is invalid").unwrap();
        let _ = lock.flush();
    }
}

fn unwrap_bool(val: &Val) -> bool {
    if let Val::Bool(b) = val {
        *b
    } else {
        panic!("Runtime error: Bool expected, got {}", val.typename());
    }
}

fn unwrap_num(val: &Val) -> IntType {
    if let Val::Num(n) = val {
        *n
    } else {
        panic!("Runtime error: Num expected, got {}", val.typename());
    }
}

fn unwrap_str(val: &Val) -> String {
    if let Val::Str(s) = val {
        s.clone()
    } else {
        panic!("Runtime error: Str expected, got {}", val.typename());
    }
}

pub fn run(prog: Ast) {
    let mut rt = Runtime::new();

    let mut i = 1; // index 0 is reserved (unreachable)
    let mut if_eval = false;
    let mut breaking = false;

    while i < prog.stmts.len() {
        match &prog.stmts[i] {
            Statement::Print { args } => {
                rt.exec_print(args);
            }
            Statement::Sub { sub, offset_to_end } => {
                let var_sub = val::Sub {
                    start: i,
                    args: sub.args.clone(),
                    res_type: sub.res_type.clone(),
                };
                rt.decl_var(sub.name.clone(), var_sub.into());
                i += offset_to_end;
            }
            Statement::Call(sub) => {
                i = rt.exec_call(sub, i);
            }
            Statement::While {
                cond,
                offset_to_end,
            } => {
                if breaking {
                    // break was fired, jump to the End
                    breaking = false;
                    i += offset_to_end;
                } else {
                    let val = cond.eval(&rt).unwrap_or_else(|e| {
                        // FIXME
                        die!("Runtime error: failed to eval condition of While : {}", e);
                    });

                    if unwrap_bool(&val) {
                        // condition was met, push a scope
                        // when reached to end, pop the scope and come here
                        rt.push(ScopeKind::Loop, i);
                    } else {
                        // condition wasn't met, jump to the End
                        i += offset_to_end;
                    }
                }
            }
            Statement::Let { name, init, .. } => {
                let init_val = init.eval(&rt).unwrap_or_else(|e| {
                    die!("Runtime error: Failed to eval init value of Let: {}", e);
                });
                rt.decl_var(name.clone(), init_val);
            }
            Statement::Modify { target, expr } => {
                let to_value = expr.eval(&rt).unwrap_or_else(|e| {
                    // FIXME
                    die!("Runtime error: Failed to eval value of Modify: {}", e);
                });
                rt.modify_var(target, to_value);
            }
            Statement::If {
                cond,
                offset_to_next,
            } => {
                // use a scope, but don't use a return address
                // push a frame always to unify End behavior
                rt.push(ScopeKind::Branch, 0);
                let val = cond.eval(&rt).unwrap_or_else(|e| {
                    // FIXME
                    die!("Runtime error: Failed to eval condition of If: {}", e);
                });
                if unwrap_bool(&val) {
                    // go to body
                    // no-op
                } else {
                    // jump to the next Elif/Else/End
                    i += offset_to_next;
                    if_eval = true;
                    continue;
                }
            }
            Statement::ElIf {
                cond,
                offset_to_next,
                ..
            } => {
                if if_eval {
                    // jumped from If/Elif
                    let val = cond.eval(&rt).unwrap_or_else(|e| {
                        // FIXME
                        die!("Runtime error: Failed to eval condition of Elif: {}", e);
                    });
                    if unwrap_bool(&val) {
                        // don't push a frame as If alread pushed one
                        if_eval = false;
                    } else {
                        // go to the next Elif/Else/End
                        i += offset_to_next;
                        continue;
                    }
                } else {
                    // come from a block
                    // jump to the End
                    i += offset_to_next;
                    continue;
                }
            }
            Statement::Else { offset_to_end, .. } => {
                if if_eval {
                    // jumped from If/Elif
                    // don't push a frame as If alread pushed one
                    if_eval = false;
                } else {
                    // come from a block
                    i += offset_to_end;
                    continue;
                }
            }
            Statement::End => {
                if_eval = false;
                let top = rt.pop().map(|s| s.ret_idx);
                match top {
                    Some(0) => {
                        // return address unspecified
                        // no-op
                    }
                    Some(ret_idx) => {
                        // return to the specified address
                        i = ret_idx;
                        continue;
                    }
                    _ => {
                        panic!("Runtime error: scope stack is empty");
                    }
                }
            }
            Statement::Input {
                prompt,
                target,
                as_num,
            } => {
                rt.exec_input(prompt, target, *as_num);
            }
            Statement::Roll {
                count,
                faces,
                target,
            } => {
                rt.exec_roll(count, faces, target);
            }
            Statement::Halt => {
                return;
            }
            Statement::Break => {
                i = loop {
                    if let Some(scope) = rt.pop() {
                        match scope.kind {
                            ScopeKind::Loop => {
                                breaking = true;
                                // go to starting stmt
                                break scope.ret_idx;
                            }
                            ScopeKind::Branch => {
                                // break the outer scope
                            }
                            _ => panic!("unexpected scope kind for Break: {:?}", scope.kind),
                        }
                    } else {
                        die!("Runtime error: scope stack is empty");
                    }
                };
                continue;
            }
            Statement::Assert { mesg, cond } => {
                let mesg_str = unwrap_str(&mesg.eval(&rt).unwrap_or_else(|e| {
                    die!("Runtime error: Failed to eval message in Assert: {}", e);
                }));

                let success = unwrap_bool(&cond.eval(&rt).unwrap_or_else(|e| {
                    die!("Runtime error: Failed to eval condition in Assert: {}", e);
                }));

                if !success {
                    die!("Runtime error: Assert \"{}\" failed", mesg_str);
                }
            }
            Statement::Continue => {
                i = loop {
                    if let Some(scope) = rt.pop() {
                        match scope.kind {
                            ScopeKind::Loop => {
                                break scope.ret_idx;
                            }
                            ScopeKind::Branch => {
                                // break the outer scope
                            }
                            _ => panic!("unexpected scope kind for Continue: {:?}", scope.kind),
                        }
                    } else {
                        panic!("Runtime error: scope stack is empty");
                    }
                };
                continue;
            }
            Statement::For {
                counter,
                from,
                to,
                offset_to_end,
            } => {
                if breaking {
                    // remove wrapper scope
                    let s = rt.pop();
                    debug_assert!(matches!(
                        s,
                        Some(Scope {
                            kind: ScopeKind::ForWrap,
                            ..
                        })
                    ));

                    // break was fired, jump to the End
                    breaking = false;
                    i += offset_to_end;
                } else {
                    let on_for;
                    if let Some(scope) = rt.peek() {
                        on_for = matches!(scope.kind, ScopeKind::ForWrap);
                    } else {
                        on_for = false;
                    }

                    let to = unwrap_num(
                        &(to.eval(&rt).unwrap_or_else(|e| {
                            die!("Runtime error: failed to eval to of For: {}", e);
                        })),
                    );

                    if on_for {
                        let cnt = { unwrap_num(rt.get_var_mut(counter)) };

                        if cnt >= to {
                            // loop ended
                            i += offset_to_end;
                        } else {
                            // loop continues
                            *rt.get_var_mut(counter) = Val::Num(cnt + 1);
                            rt.push(ScopeKind::Loop, i);
                        }
                    } else {
                        // create counter
                        rt.push(ScopeKind::ForWrap, i);
                        let from = unwrap_num(&from.eval(&rt).unwrap_or_else(|e| {
                            die!("Runtime error: failed to eval from of For: {}", e);
                        }));

                        if from > to {
                            // loop ended
                            i += offset_to_end;
                        } else {
                            rt.decl_var(counter.clone(), Val::Num(from));
                            rt.push(ScopeKind::Loop, i);
                        }
                    }
                }
            }
            Statement::Return(res) => {
                let res_val = res.as_ref().map(|expr| {
                    expr.eval(&rt).unwrap_or_else(|e| {
                        die!("Runtime error: failed to eval the return value: {}", e);
                    })
                });

                // use loop to recursively exit scopes
                // (e.g. Return can be happen in a If in a Sub)
                let (ret_idx, target) = loop {
                    if let Some(scope) = rt.pop() {
                        if let ScopeKind::Sub(target) = scope.kind {
                            let target =
                                target.map(|tgt| rt.resolve_target(&tgt.0.into()).unwrap());
                            break (scope.ret_idx, target);
                        }
                    } else {
                        die!("Runtime error: scope stack is empty");
                    }
                };

                // assign the result to the target
                match (target, res_val) {
                    (None, _) => { /* OK */ }
                    (Some(target_resolved), Some(val)) => {
                        *target_resolved = val;
                    }
                    (Some(_), None) => unreachable!("The callee has no result"),
                }
                i = ret_idx;
                continue;
            }
            Statement::Ill => panic!("Unreachable statement"),
        }
        i += 1;
    }
}
