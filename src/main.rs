use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "line.pest"]
struct LineParser;

enum LineType {
    Print(String),
}

fn parse_lines(s: String) -> Option<Vec<LineType>> {
    let lines = LineParser::parse(Rule::Lines, &s);
    if let Err(e) = lines {
        eprintln!("{}", e);
        return None;
    }

    let mut res = vec![];
    for line in lines.unwrap() {
        //dbg!(&line);
        match line.as_rule() {
            Rule::PrintLine => res.push(LineType::Print(
                line.into_inner()
                    .next()
                    .unwrap()
                    .into_inner()
                    .as_str()
                    .to_owned(),
            )),
            _ => unreachable!(),
        }
    }
    Some(res)
}

pub fn wait_keypress() {
    use crossterm::event::*;
    loop {
        if let Event::Key(KeyEvent { .. }) = read().unwrap() {
            return;
        }
    }
}

fn process_lines<T: IntoIterator<Item = LineType>>(parsed: T) {
    use std::io::Write;
    for line in parsed {
        match line {
            LineType::Print(txt) => {
                crossterm::execute!(
                    std::io::stdout(),
                    crossterm::style::Print(format!("{}\r\n[Proceed with any key]\r", txt)),
                );
                let _ = wait_keypress();
            }
            _ => unreachable!(),
        }
    }
}

fn main() {
    let path = std::env::args().nth(1).unwrap_or_else(|| {
        eprintln!("filename was not provided");
        std::process::exit(1);
    });
    let s = std::fs::read_to_string(path).unwrap();

    eprint!("Loading the file...");
    let parsed = parse_lines(s);
    eprintln!(" completed");

    if let Some(i) = parsed {
        ctrlc::set_handler(|| {
            let _ = crossterm::terminal::disable_raw_mode();
        })
        .unwrap();
        let _ = crossterm::terminal::enable_raw_mode();
        process_lines(i);
        let _ = crossterm::terminal::disable_raw_mode();
    } else {
        std::process::exit(1);
    }
}
