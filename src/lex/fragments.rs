use super::utils::*;
use super::*;
pub(super) fn handle_multichars(vs: &[char]) -> Result<(LangItem, usize), ErrorKind> {
    if let Some(inner) = handle_reserved(vs) {
        Ok(inner)
    } else if vs[0].is_numeric() {
        Ok(handle_number(vs)?)
    } else if is_ident_char(vs[0]) {
        Ok(handle_ident(vs))
    } else {
        Err(ErrorKind::UnexpectedChar(vs[0]))
    }
}

fn handle_reserved(vs: &[char]) -> Option<(LangItem, usize)> {
    if let Some(res) = Keyword::parse_slice(vs) {
        let len = res.len();
        Some((LangItem::Key(res), len))
    } else if let Some(res) = Command::parse_slice(vs) {
        let len = res.len();
        Some((LangItem::Cmd(res), len))
    } else if let Some(res) = Ops::parse_slice(vs) {
        let len = res.len();
        Some((LangItem::Op(res), len))
    } else {
        None
    }
}

fn handle_ident(vs: &[char]) -> (LangItem, usize) {
    let mut len = 0;
    while len < vs.len() && is_ident_char(vs[len]) {
        len += 1;
    }
    (
        LangItem::Ident(vs[0..len].iter().collect::<String>().into()),
        len,
    )
}

fn handle_number(vs: &[char]) -> Result<(LangItem, usize), ErrorKind> {
    let mut len = 0;
    while len < vs.len() && vs[len].is_numeric() {
        len += 1;
    }
    let s = String::from_iter(&vs[0..len]);
    match s.parse() {
        Ok(i) => Ok((LangItem::Num(i, len), len)),
        Err(_) => Err(ErrorKind::TooLongNum),
    }
}

pub(super) fn handle_string(vs: &[char]) -> Result<(LangItem, usize), ErrorKind> {
    let mut len = 0;
    loop {
        if len >= vs.len() {
            return Err(ErrorKind::UnterminatedStr);
        }
        if vs[len] == '"' {
            let s = vs[0..len].iter().collect();
            return Ok((LangItem::Str(s), len));
        }
        len += 1;
    }
}
