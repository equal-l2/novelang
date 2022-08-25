use clap::Parser;
use novelang::{die, Location, Range};

#[derive(clap::Parser)]
struct Opt {
    filename: String,
    #[clap(long)]
    compile: bool, // don't run
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

#[derive(Debug, Clone)]
pub struct LocWithLine {
    line: String,
    loc: Location,
}

impl std::fmt::Display for LocWithLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let row = self.loc.row;
        let col = self.loc.col;
        writeln!(f, "     |")?;
        writeln!(f, "{:<4} | {}", row, self.line)?;
        writeln!(f, "     | {:>1$}", "^", col)?;
        writeln!(f, "     |")?;
        Ok(())
    }
}

// TODO: implement Display wrapper for novelang::Error
pub fn generate_source_info<S: AsRef<str>>(lines: &[S], range: Range) -> String {
    use std::fmt::Write;
    let mut s = String::new();

    eprintln!("{:?}", range);

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
    writeln!(s, "     |").unwrap();
    writeln!(s, "{:<4} | {}", row, line).unwrap();
    writeln!(
        s,
        "     | {:>pad$}{:~>len$}",
        "",
        "",
        pad = col - 1,
        len = len
    )
    .unwrap();
    //writeln!(s, "     | {:>pad$}", "^", pad = col).unwrap();
    writeln!(s, "     |").unwrap();

    s
}

fn print_compile_errors<S: AsRef<str>>(e: novelang::Error, lines: &[S]) {
    match e {
        novelang::Error::Lex(e, r) => {
            let info = generate_source_info(lines, r);
            eprintln!("Lex error: {}\n{}", e, info)
        }
        novelang::Error::Parse(e, r) => {
            let info = generate_source_info(lines, r);
            eprintln!("Parse error: {}\n{}", e, info)
        }
        novelang::Error::Block(es) => {
            for (e, r) in es {
                let info = generate_source_info(lines, r);
                eprintln!("Block syntax error: {}\n{}", e, info)
            }
        }
        novelang::Error::Semck(es) => {
            for (e, r) in es {
                let info = generate_source_info(lines, r);
                eprintln!("Semantic error: {}\n{}", e, info)
            }
        }
    }
}

fn main() {
    env_logger::init();
    let opt = Opt::parse();
    let lines = read_input(&opt.filename).unwrap_or_else(|e| die!("Input error: {}", e));

    let insts = novelang::compile(&lines).unwrap_or_else(|e| {
        print_compile_errors(e, &lines);
        die!("Exiting...");
    });
    eprintln!("Successfully compiled");

    if !opt.compile {
        novelang::run(insts);
    }
}
