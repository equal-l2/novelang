use clap::Parser;
use novelang::{die, Range};

#[derive(clap::Parser)]
struct Opt {
    filename: String,
    #[clap(long)]
    check: bool, // don't run
}

#[derive(Debug)]
enum InputError<'a> {
    Stdin(std::io::Error),
    File(&'a str, std::io::Error),
}

impl<'a> std::fmt::Display for InputError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Stdin(e) => write!(f, "failed to read stdin ({})", e),
            Self::File(name, e) => write!(f, "failed to read \"{}\" ({})", name, e),
        }
    }
}

impl<'a> std::error::Error for InputError<'a> {}

fn read_input(filename: &str) -> Result<Vec<String>, InputError<'_>> {
    if filename == "-" {
        std::io::stdin()
            .lines()
            .collect::<Result<_, _>>()
            .map_err(InputError::Stdin)
    } else {
        std::io::BufRead::lines(std::io::BufReader::new(
            std::fs::File::open(filename).map_err(|e| InputError::File(filename, e))?,
        ))
        .collect::<Result<_, _>>()
        .map_err(|e| InputError::File(filename, e))
    }
}

// TODO: implement Display wrapper for novelang::Error
pub fn generate_source_info<S: AsRef<str>>(lines: &[S], range: Range) -> String {
    const BRIGHT_BLUE: &str = "\x1B[94m";
    const BRIGHT_RED: &str = "\x1B[91m";
    const RESET: &str = "\x1B[0m";

    use std::fmt::Write;

    // TODO: use isatty to switch them no-op
    let bright_blue = BRIGHT_BLUE;
    let bright_red = BRIGHT_RED;
    let reset = RESET;

    let mut s = String::new();

    let Range(head, tail) = range;
    let row = head.row;
    let line = lines[row - 1].as_ref();

    let len = if head.row == tail.row {
        tail.col - head.col
    } else {
        let line_len = line.len();
        line_len - head.col
    };

    let col = head.col;
    write!(s, "{}", bright_blue).unwrap();
    writeln!(s, "     |").unwrap();

    write!(s, "{:<4} | ", row).unwrap();
    write!(s, "{}", reset).unwrap();
    writeln!(s, "{}", line).unwrap();

    write!(s, "{}", bright_blue).unwrap();
    write!(s, "     | {:>pad$}", "", pad = col - 1,).unwrap();
    write!(s, "{}", bright_red).unwrap();
    writeln!(s, "{:~>len$}", "", len = len).unwrap();
    //writeln!(s, "     | {:>pad$}", "^", pad = col).unwrap();

    write!(s, "{}", reset).unwrap();

    s
}

pub fn error_count(e: &novelang::Error) -> usize {
    use novelang::Error::*;
    match e {
        Lex(v) => v.len(),
        Block(v) => v.len(),
        Semck(v) => v.len(),
        Parse(..) => 1,
    }
}

fn print_compile_errors<S: AsRef<str>>(e: novelang::Error, lines: &[S]) {
    use novelang::Error::*;
    match e {
        Lex(es) => {
            println!("{}", es.len());
            for (e, r) in es {
                let info = generate_source_info(lines, r);
                eprintln!("Lex syntax error: {}\n{}", e, info)
            }
        }
        Parse(e, r) => {
            let info = generate_source_info(lines, r);
            eprintln!("Parse error: {}\n{}", e, info)
        }
        Block(es) => {
            for (e, r) in es {
                let info = generate_source_info(lines, r);
                eprintln!("Block syntax error: {}\n{}", e, info)
            }
        }
        Semck(es) => {
            for (e, r) in es {
                let info = generate_source_info(lines, r);
                eprintln!("Semantic error: {}\n{}", e, info)
            }
        }
    }
}

fn main() {
    let opt = Opt::parse();
    let lines = read_input(&opt.filename).unwrap_or_else(|e| die!("Input error: {}", e));

    let insts = novelang::compile(&lines).unwrap_or_else(|e| {
        let cnt = error_count(&e);
        print_compile_errors(e, &lines);
        die!("{cnt} error(s) found, exiting...");
    });

    if opt.check {
        eprintln!("Successfully compiled");
    } else {
        novelang::run(insts);
    }
}
