use clap::Parser;
use novelang::die;

#[derive(clap::Parser)]
struct Opt {
    filename: String,
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

fn read_input(filename: &str) -> Result<String, InputError<'_>> {
    if filename == "-" {
        let mut s = String::new();
        std::io::Read::read_to_string(&mut std::io::stdin(), &mut s).map_err(InputError::Stdin)?;
        Ok(s)
    } else {
        std::fs::read_to_string(filename).map_err(|e| InputError::File(filename, e))
    }
}

fn main() {
    env_logger::init();
    let opt = Opt::parse();
    let s = read_input(&opt.filename).unwrap_or_else(|e| die!("Input error: {}", e));

    let insts = novelang::compile(&s).unwrap_or_else(|e| {
        eprintln!("{e}");
        die!("Exiting...");
    });
    eprintln!("Info: Load completed");

    novelang::run(insts);
}
