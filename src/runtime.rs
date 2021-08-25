mod exprs;
mod variable;

use crate::die;
use crate::lval::LVal;
use crate::parse::{Statement, AST};
use crate::types::{IntType, Typed};

use exprs::{Eval, Expr};
use variable::{ModifyError, Variable};

type VarTable = std::collections::HashMap<String, Variable>;

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

#[derive(Debug, PartialEq, Eq)]
enum ScopeKind {
    Branch,
    Loop,
    Sub,
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
    fn get(&self, name: &str) -> &Typed {
        self.get_var(name).get()
    }

    fn get_arr_elem<L: Eval, R: Eval>(&self, l: &L, r: &R) -> exprs::Result {
        use exprs::EvalError;
        let r = r.eval(self)?;
        if let Typed::Num(n) = r {
            let l = l.eval(self)?;
            match l {
                Typed::Str(s) => {
                    if let Some(c) = s.chars().nth(n as usize) {
                        Ok(Typed::Str(c.into()))
                    } else {
                        Err(EvalError::IndexOutOfBounds(n))
                    }
                }
                Typed::Arr(v) => {
                    if let Some(i) = v.get(n as usize) {
                        Ok(i.clone())
                    } else {
                        Err(EvalError::IndexOutOfBounds(n))
                    }
                }
                _ => unreachable!("Type {} cannot be indexed", l.typename()),
            }
        } else {
            unreachable!("non-Num index is not allowed");
        }
    }
}

enum LValRef<'a> {
    Scalar(&'a mut Variable),
    Vector {
        refer: &'a mut Typed,
        is_mutable: bool,
    },
}

impl<'a> LValRef<'a> {
    fn is_mutable(&self) -> bool {
        match self {
            Self::Scalar(var) => var.is_mutable(),
            Self::Vector { is_mutable, .. } => *is_mutable,
        }
    }

    fn value(&mut self) -> &mut Typed {
        match self {
            Self::Scalar(var) => var.get_mut(),
            Self::Vector { refer, .. } => refer,
        }
    }

    fn modify(&mut self, v: Typed) -> Result<(), variable::ModifyError> {
        if self.is_mutable() {
            let val = self.value();
            match (&val, &v) {
                (Typed::Num(_), Typed::Num(_)) | (Typed::Bool(_), Typed::Bool(_)) => {
                    *val = v;
                    Ok(())
                }
                _ => Err(ModifyError::TypeDiffers),
            }
        } else {
            Err(ModifyError::Immutable)
        }
    }
}

impl Runtime {
    fn new() -> Self {
        use rand::SeedableRng;
        // internal variables
        // - "_wait": whether wait is enabled

        let internals = {
            let mut vt = VarTable::new();
            vt.insert("_wait".to_owned(), Variable::new_mut(Typed::Bool(false)));
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
    fn decl_var(&mut self, name: &str, val: Variable) {
        let var_table = if self.stack.is_empty() {
            &mut self.globals
        } else {
            &mut self.stack.last_mut().unwrap().vars
        };
        if var_table.insert(name.to_owned(), val).is_some() {
            die!("Runtime error: variable {} is already declared", name);
        }
    }

    fn resolve_lval(&mut self, target: &LVal) -> Result<LValRef<'_>, exprs::EvalError> {
        match target {
            LVal::Scalar(s) => Ok(LValRef::Scalar(self.get_var_mut(s))),
            LVal::Vector(l, r) => {
                // v[1][2] | [3]
                let r = r.eval(self)?;
                if let Typed::Num(n) = r {
                    let resolved = self.resolve_lval(l)?;
                    match resolved {
                        LValRef::Scalar(var) => {
                            let is_mutable = var.is_mutable();
                            match var.get_mut() {
                                Typed::Arr(v) => Ok(LValRef::Vector {
                                    refer: v.get_mut(n as usize).expect("God said so"),
                                    is_mutable,
                                }),
                                _ => panic!(),
                            }
                        }
                        LValRef::Vector { refer, is_mutable } => match refer {
                            Typed::Arr(v) => Ok(LValRef::Vector {
                                refer: v.get_mut(n as usize).expect("God said so!"),
                                is_mutable,
                            }),
                            _ => panic!(),
                        },
                    }
                } else {
                    unreachable!("non-Num index is not allowed");
                }
            }
        }
    }

    /// Modify a variable
    /// Aborts on error (the variable doesn't exists, differ in type, or is immutable)
    fn modify_var(&mut self, target: &LVal, val: Typed) {
        let mut var = self.resolve_lval(target).expect("variable must exist");

        match var.modify(val) {
            Ok(_) => {}
            Err(ModifyError::TypeDiffers) => {
                die!("Runtime error: Type differs");
            }
            Err(ModifyError::Immutable) => {
                die!("Runtime error: variable {} is immutable", target);
            }
        }
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
        self.stack.push(Scope::new(kind, ret_idx))
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
    pub fn get_var(&self, name: &str) -> &Variable {
        self.vars_iter() // Iterator<Item = &mut VarTable>
            .map(|t| t.get(name)) // Iterator<Item = Option<&Variable>>
            .find(Option::is_some) // Option<Option<&Variable>>
            .flatten() // Option<&Variable>
            .expect("Variable must exist")
    }

    fn get_var_mut(&mut self, name: &String) -> &mut Variable {
        self.vars_iter_mut()
            .map(|t| t.get_mut(name))
            .find(Option::is_some)
            .flatten()
            .expect("Variable must exist")
    }

    pub fn exec_roll(&mut self, cnt: &Expr, face: &Expr, name: &LVal) {
        use rand::Rng;

        let cnt = unwrap_num(&cnt.eval(self).unwrap_or_else(|e| {
            die!("Runtime error: Failed to eval count of Roll: {}", e);
        }));
        let face = unwrap_num(&face.eval(self).unwrap_or_else(|e| {
            die!("Runtime error: Failed to eval face of Roll: {}", e);
        }));

        if cnt <= 0 {
            die!("Runtime error: Count for Roll must be a positive integer");
        }

        if face <= 0 {
            die!("Runtime error: Face for Roll must be a positive integer");
        }

        let val = std::iter::repeat_with(|| self.rng.gen_range(1..=face))
            .take(cnt as usize)
            .sum();

        self.modify_var(name, Typed::Num(val));
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

            let wait = unwrap_bool(self.get_var("_wait").get());

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

    pub fn exec_input(&mut self, prompt: &Option<String>, name: &LVal, as_num: bool) {
        if as_num {
            let input = get_input(prompt.as_deref());
            self.modify_var(name, Typed::Num(input));
        } else {
            let input = get_input(prompt.as_deref());
            self.modify_var(name, Typed::Str(input));
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

fn unwrap_bool(val: &Typed) -> bool {
    if let Typed::Bool(b) = val {
        *b
    } else {
        panic!("Runtime error: Bool expected, got {}", val.typename());
    }
}

fn unwrap_num(val: &Typed) -> IntType {
    if let Typed::Num(n) = val {
        *n
    } else {
        panic!("Runtime error: Num expected, got {}", val.typename());
    }
}

fn unwrap_sub(val: &Typed) -> usize {
    if let Typed::Sub(n) = val {
        *n
    } else {
        panic!("Runtime error: Sub expected, got {}", val.typename());
    }
}

fn unwrap_str(val: &Typed) -> String {
    if let Typed::Str(s) = val {
        s.clone()
    } else {
        panic!("Runtime error: Str expected, got {}", val.typename());
    }
}

pub fn run(prog: AST) {
    let mut rt = Runtime::new();

    let mut i = 1; // index 0 is reserved (unreachable)
    let mut if_eval = false;
    let mut breaking = false;

    while i < prog.stmts.len() {
        match &prog.stmts[i] {
            Statement::Print { args } => {
                rt.exec_print(args);
            }
            Statement::Sub {
                name,
                offset_to_end,
            } => {
                rt.decl_var(name, Variable::new(Typed::Sub(i)));
                i += offset_to_end;
            }
            Statement::Call { name } => {
                let idx = unwrap_sub(rt.get_var(name).get());

                // register address to return (the next line)
                rt.push(ScopeKind::Sub, i + 1);

                // jump to the address of the sub
                i = idx;
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
            Statement::Let { name, init, is_mut } => {
                // no check for internals, as already checked in the parse phase.
                let init_val = init.eval(&rt).unwrap_or_else(|e| {
                    die!("Runtime error: Failed to eval init value of Let: {}", e);
                });
                rt.decl_var(
                    name,
                    if *is_mut {
                        Variable::new_mut(init_val)
                    } else {
                        Variable::new(init_val)
                    },
                );
            }
            Statement::Modify { target, expr } => {
                // no check for internals, as already checked in the parse phase.
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
                face,
                target,
            } => {
                rt.exec_roll(count, face, target);
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
                            ScopeKind::Sub => {
                                panic!("Runtime error: cannot break in sub scope");
                            }
                            ScopeKind::Branch => {
                                // break the outer scope
                            }
                            _ => panic!("unexpected scope kind: {:?}", scope.kind),
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
                            ScopeKind::Sub => {
                                panic!("Runtime error: cannot continue in sub scope");
                            }
                            ScopeKind::Branch => {
                                // break the outer scope
                            }
                            _ => panic!("unexpected scope kind: {:?}", scope.kind),
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
                        on_for = scope.kind == ScopeKind::ForWrap;
                    } else {
                        on_for = false;
                    }

                    let to = unwrap_num(
                        &(to.eval(&rt).unwrap_or_else(|e| {
                            die!("Runtime error: failed to eval to of For: {}", e);
                        })),
                    );

                    if !on_for {
                        // create counter
                        rt.push(ScopeKind::ForWrap, i);
                        let from = unwrap_num(&from.eval(&rt).unwrap_or_else(|e| {
                            die!("Runtime error: failed to eval from of For: {}", e);
                        }));

                        if from > to {
                            // loop ended
                            i += offset_to_end;
                        } else {
                            rt.decl_var(counter, Variable::new(Typed::Num(from)));
                            rt.push(ScopeKind::Loop, i);
                        }
                    } else {
                        let cnt = { unwrap_num(&rt.get_var_mut(counter).get()) };

                        if cnt >= to {
                            // loop ended
                            i += offset_to_end;
                        } else {
                            // loop continues
                            rt.get_var_mut(counter).force_modify(Typed::Num(cnt + 1));
                            rt.push(ScopeKind::Loop, i);
                        }
                    }
                }
            }
            Statement::Return => {
                i = loop {
                    if let Some(scope) = rt.pop() {
                        match scope.kind {
                            ScopeKind::Sub => {
                                break scope.ret_idx;
                            }
                            _ => {}
                        }
                    } else {
                        die!("Runtime error: scope stack is empty");
                    }
                };
                continue;
            }
            Statement::Ill => panic!("Unreachable statement"),
        }
        i += 1;
    }
}
