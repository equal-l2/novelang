use super::Program;
use super::StmtType;
use std::collections::HashMap;

struct CallFrame {
    ret_idx: usize,
    vars: HashMap<String, Variable>,
}

impl CallFrame {
    fn new(ret_idx: usize) -> Self {
        Self { ret_idx, vars: HashMap::new() }
    }
}

struct Variable {
    is_mutable: bool,
    value: usize,
}

fn process_stmts(prog: Program) {
    use std::io::Write;
    //let mut global_vars = HashMap::new();
    let mut call_stack = vec![];
    let mut i = 0;
    while i < prog.stmts.len() {
        match &prog.stmts[i] {
            StmtType::Print { text } => {
                crossterm::execute!(
                    std::io::stdout(),
                    crossterm::terminal::Clear(crossterm::terminal::ClearType::CurrentLine),
                    crossterm::style::Print(format!(
                        "{:04} : {}\r\n[Proceed with any key]\r",
                        i, text
                    ))
                )
                .unwrap();
                let _ = wait_keypress();
                i += 1;
            }
            StmtType::FnBegin { offset_to_end, .. } => {
                i += offset_to_end + 1;
            }
            StmtType::Call { name } => {
                if let Some(idx) = prog.fns.get(name) {
                    call_stack.push(CallFrame::new(i + 1));
                    i = *idx + 1;
                } else {
                    let _ = crossterm::terminal::disable_raw_mode();
                    eprintln!("Runtime error: function \"{}\" was not found", name);
                    std::process::exit(1);
                }
            }
            StmtType::FnEnd => {
                if let Some(frame) = call_stack.pop() {
                    i = frame.ret_idx;
                } else {
                    unreachable!()
                }
            }
            _ => unreachable!(),
        }
    }
}

pub fn run(prog: Program) {
    ctrlc::set_handler(|| {
        let _ = crossterm::terminal::disable_raw_mode();
    })
    .unwrap();
    let _ = crossterm::terminal::enable_raw_mode();
    process_stmts(prog);
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
