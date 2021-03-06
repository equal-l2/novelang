mod variable;

use crate::die;
use crate::exprs;
use crate::parse::{Statement, AST};
use crate::types::{IntType, Typed};

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

enum ScopeKind {
    Branch,
    Loop,
    Sub,
}

/// Represents the store for runtime state
pub struct Runtime {
    stack: Vec<Scope>,
    globals: VarTable,
    internals: VarTable,
}

impl crate::exprs::VarsMap for Runtime {
    fn get(&self, name: &str) -> Option<&Typed> {
        self.get_var(name).map(Variable::get)
    }
}

impl Runtime {
    fn new() -> Self {
        // internal variables
        // - "_wait": whether wait is enabled

        let internals = {
            let mut vt = VarTable::new();
            vt.insert("_wait".to_owned(), Variable::new_mut(Typed::Bool(false)));
            vt
        };

        Self {
            stack: vec![],
            globals: VarTable::new(),
            internals,
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

    /// Modify a variable
    /// Aborts on error (the variable doesn't exists, differ in type, or is immutable)
    fn modify_var(&mut self, name: &str, val: Typed) {
        // no check for internals as already done in the parse phase.

        let var = self.get_var_mut(name).unwrap_or_else(|| {
            die!("Runtime error: variable was not found");
        });

        match var.modify(val) {
            Ok(_) => {}
            Err(ModifyError::TypeDiffers) => {
                die!("Runtime error: Type differs");
            }
            Err(ModifyError::Immutable) => {
                die!("Runtime error: variable {} is immutable", name);
            }
        }
    }

    /// Pop the current scope
    fn pop(&mut self) -> Option<Scope> {
        self.stack.pop()
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
    pub fn get_var(&self, name: &str) -> Option<&Variable> {
        self.vars_iter() // Iterator<Item = &mut VarTable>
            .map(|t| t.get(name)) // Iterator<Item = Option<&Variable>>
            .find(Option::is_some) // Option<Option<&Variable>>
            .flatten() // Option<&Variable>
    }

    fn get_var_mut(&mut self, name: &str) -> Option<&mut Variable> {
        self.vars_iter_mut()
            .map(|t| t.get_mut(name))
            .find(Option::is_some)
            .flatten()
    }

    fn eval(&self, expr: &exprs::Expr) -> Result<Typed, exprs::EvalError> {
        expr.eval_on(self)
    }
}

fn exec_print(idx: usize, runtime: &Runtime, wait: bool, args: &[exprs::Expr]) {
    use std::io::Write;
    let stdout = std::io::stdout();
    let mut lock = stdout.lock();

    write!(lock, "{:04} :", idx).unwrap();
    for arg in args {
        let val = arg.eval_on(runtime).unwrap_or_else(|e| {
            die!("Runtime error: Failed to eval arg of Print: {:?}", e);
        });
        match val {
            Typed::Num(n) => write!(lock, " {}", n),
            Typed::Bool(b) => write!(lock, " {}", b),
            Typed::Str(s) => write!(lock, " {}", s),
            _ => unimplemented!(),
        }
        .unwrap();
    }
    writeln!(lock).unwrap();
    let _ = lock.flush();

    if wait {
        write!(lock, "[Proceed with Enter⏎ ]").unwrap();
        let _ = lock.flush();
        let _ = read_line_from_stdin();
        {
            use crossterm::cursor;
            use crossterm::execute;
            use crossterm::terminal;
            execute!(
                lock,
                cursor::MoveToPreviousLine(1),
                terminal::Clear(terminal::ClearType::CurrentLine)
            )
            .unwrap();
        }
    }
}

fn get_int_input(prompt: Option<&str>) -> IntType {
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
        die!("Runtime error: Bool expected, got {}", val.typename());
    }
}

fn unwrap_num(val: &Typed) -> IntType {
    if let Typed::Num(n) = val {
        *n
    } else {
        die!("Runtime error: Num expected, got {}", val.typename());
    }
}

fn unwrap_sub(val: &Typed) -> usize {
    if let Typed::Sub(n) = val {
        *n
    } else {
        die!("Runtime error: Sub expected, got {}", val.typename());
    }
}

pub fn run(prog: AST) {
    let mut runtime = Runtime::new();

    let mut i = 1; // index 0 is reserved (unreachable)
    let mut if_eval = false;
    let mut breaking = false;

    while i < prog.stmts.len() {
        match &prog.stmts[i] {
            Statement::Print { args } => {
                exec_print(
                    i,
                    &runtime,
                    unwrap_bool(runtime.get_var("_wait").unwrap().get()),
                    args,
                );
            }
            Statement::Sub {
                name,
                offset_to_end,
            } => {
                runtime.decl_var(name, Variable::new(Typed::Sub(i)));
                i += offset_to_end;
            }
            Statement::Call { name } => {
                if let Some(idx) = runtime.get_var(name) {
                    let idx = unwrap_sub(idx.get());

                    // register address to return (the next line)
                    runtime.push(ScopeKind::Sub, i + 1);

                    // jump to the address of the sub
                    i = idx;
                } else {
                    die!("Runtime error: function \"{}\" was not found", name);
                }
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
                    let val = runtime.eval(cond).unwrap_or_else(|e| {
                        // FIXME
                        die!("Runtime error: failed to eval condition of While : {}", e);
                    });

                    if unwrap_bool(&val) {
                        // condition was met, push a scope
                        // when reached to end, pop the scope and come here
                        runtime.push(ScopeKind::Loop, i);
                    } else {
                        // condition wasn't met, jump to the End
                        i += offset_to_end;
                    }
                }
            }
            Statement::Let { name, init, is_mut } => {
                // no check for internals, as already checked in the parse phase.
                let init_val = runtime.eval(init).unwrap_or_else(|e| {
                    die!("Runtime error: Failed to eval init value of Let: {}", e);
                });
                runtime.decl_var(
                    name,
                    if *is_mut {
                        Variable::new_mut(init_val)
                    } else {
                        Variable::new(init_val)
                    },
                );
            }
            Statement::Modify { name, expr } => {
                // no check for internals, as already checked in the parse phase.
                let to_value = runtime.eval(expr).unwrap_or_else(|e| {
                    // FIXME
                    die!("Runtime error: Failed to eval value of Modify: {}", e);
                });
                runtime.modify_var(name, to_value);
            }
            Statement::If {
                cond,
                offset_to_next,
            } => {
                // use a scope, but don't use a return address
                // push a frame always to unify End behavior
                runtime.push(ScopeKind::Branch, 0);
                let val = runtime.eval(cond).unwrap_or_else(|e| {
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
                    let val = runtime.eval(cond).unwrap_or_else(|e| {
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
                let top = runtime.pop().map(|s| s.ret_idx);
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
                        die!("Runtime error: scope stack is empty");
                    }
                }
            }
            Statement::Input {
                prompt,
                name,
                as_num,
            } => {
                if *as_num {
                    runtime.modify_var(name, Typed::Num(get_int_input(prompt.as_deref())));
                } else {
                    todo!()
                }
            }
            Statement::Roll { count, face, name } => {
                let count = unwrap_num(&runtime.eval(count).unwrap_or_else(|e| {
                    die!("Runtime error: Failed to eval count of Roll: {}", e);
                }));
                let face = unwrap_num(&runtime.eval(face).unwrap_or_else(|e| {
                    die!("Runtime error: Failed to eval face of Roll: {}", e);
                }));

                if count <= 0 {
                    die!("Runtime error: Count for Roll must be a positive integer");
                }

                if face <= 0 {
                    die!("Runtime error: Face for Roll must be a positive integer");
                }
                runtime.modify_var(name, Typed::Num(roll_dice(count, face)));
            }
            Statement::Halt => {
                return;
            }
            Statement::Break => {
                i = loop {
                    if let Some(scope) = runtime.pop() {
                        match scope.kind {
                            ScopeKind::Loop => {
                                breaking = true;
                                break scope.ret_idx;
                            }
                            ScopeKind::Sub => {
                                break scope.ret_idx;
                            }
                            ScopeKind::Branch => {
                                // break the outer scope
                            }
                        }
                    } else {
                        die!("Runtime error: scope stack is empty");
                    }
                };
                continue;
            }
            #[allow(unreachable_patterns)]
            other => {
                die!("Runtime error: unknown instruction: {:?}", other);
            }
        }
        i += 1;
    }
}

fn read_line_from_stdin() -> String {
    use std::io::BufRead;
    let stdin = std::io::stdin();
    let mut it = stdin.lock().lines();
    it.next().unwrap_or_else(|| Ok("".to_owned())).unwrap()
}

fn roll_dice(count: IntType, face: IntType) -> IntType {
    use rand::Rng;
    let mut rng = rand::thread_rng();
    let mut sum = 0;

    for _ in 0..count {
        sum += rng.gen_range(1..=face);
    }

    sum
}
