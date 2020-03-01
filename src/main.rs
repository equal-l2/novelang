mod exprs;
mod parser;
mod runner;

use structopt::StructOpt;

#[derive(StructOpt)]
struct Opt {
    #[structopt(long)]
    wait: bool,
    filename: String,
}

fn main() {
    let opt = Opt::from_args();
    let s = std::fs::read_to_string(opt.filename).unwrap_or_else(|e| {
        eprintln!("File error: file could not be read : {}", e);
        std::process::exit(1);
    });

    eprintln!("Info: loading the file");
    let parsed = parser::parse(s);
    eprintln!("Info: load completed");

    if let Some(i) = parsed {
        runner::run(i, opt.wait);
    } else {
        std::process::exit(1);
    }
}
