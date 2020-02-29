use crate::parser::Program;
use crate::parser::Inst;
use std::collections::HashMap;

pub type VarTable = HashMap<String, Variable>;

pub struct CallStack {
    ret_stack: Vec<usize>,
    vars_stack: Vec<VarTable>,
}

impl CallStack {
    fn new() -> Self {
        // init with one vartable for global vars
        Self { ret_stack: vec![], vars_stack: vec![VarTable::new()] }
    }

    fn pop(&mut self) -> Option<usize> {
        self.vars_stack.pop().unwrap();
        self.ret_stack.pop()
    }

    fn push(&mut self, ret_index: usize) {
        self.vars_stack.push(Default::default());
        self.ret_stack.push(ret_index)
    }

    // get the highest variable in the stack with the specified name
    pub fn get_var(&self, name: &String) -> Option<&Variable> {
        for table in self.vars_stack.iter().rev() {
            if let Some(v) = table.get(name) {
                return Some(v);
            }
        }
        None
    }

    fn get_var_mut(&mut self, name: &String) -> Option<&mut Variable> {
        for table in self.vars_stack.iter_mut().rev() {
            if let Some(v) = table.get_mut(name) {
                return Some(v);
            }
        }
        None
    }
}

pub struct Variable {
    is_mutable: bool,
    pub value: usize,
}

macro_rules! die {
    ($( $x:expr ),*) => {
        let _ = crossterm::terminal::disable_raw_mode();
        eprintln!($($x,)*);
        std::process::exit(1);
    }
}

fn process_stmts(prog: Program, wait: bool) {
    use std::io::Write;

    let mut call_stack = CallStack::new();
    let mut i = 0;
    while i < prog.stmts.len() {
        match &prog.stmts[i] {
            Inst::Print { text } => {
                crossterm::execute!(
                    std::io::stdout(),
                    crossterm::terminal::Clear(crossterm::terminal::ClearType::CurrentLine),
                    crossterm::style::Print(format!(
                        "{:04} : {}\r\n[Proceed with any key]\r",
                        i, text
                    ))
                )
                .unwrap();
                if wait {let _ = wait_keypress();}
                i += 1;
            }
            Inst::Sub { offset_to_end, .. } => {
                i += offset_to_end + 1;
            }
            Inst::Call { name } => {
                if let Some(idx) = prog.subs.get(name) {
                    call_stack.push(i + 1);
                    i = *idx + 1;
                } else {
                    die!("Runtime error: function \"{}\" was not found", name);
                }
            }
            Inst::End => {
                if let Some(ret_idx) = call_stack.pop() {
                    i = ret_idx;
                } else {
                    die!("Runtime error: index to return was not set");
                }
            }
            Inst::While { cond, offset_to_end } => {
                use crate::exprs::Eval;
                if let Some(b) = cond.eval(&call_stack) {
                    if b {
                        call_stack.push(i);
                        i += 1;
                    } else {
                        i += offset_to_end + 1;
                    }
                } else {
                    die!("Runtime error: condition expression is corrupted");
                }
            }
            other => {
                die!("Runtime error: unknown instruction type : {:?}", other);
            }
        }
    }
}

pub fn run(prog: Program, wait: bool) {
    ctrlc::set_handler(|| {
        let _ = crossterm::terminal::disable_raw_mode();
        std::process::exit(-1);
    })
    .unwrap();
    if wait { let _ = crossterm::terminal::enable_raw_mode(); }
    process_stmts(prog, wait);
    let _ = crossterm::terminal::disable_raw_mode();
}

fn wait_keypress() {
    use crossterm::event::*;
    loop {
        if let Event::Key(KeyEvent { .. }) = read().unwrap() {
            return;
        }
    }
}
