pub fn is_item(item_chars: &[char], src_chars: &[char]) -> bool {
    item_chars.len() <= src_chars.len()
        && item_chars
            .iter()
            .zip(src_chars)
            .all(|(i, s)| i.to_lowercase().eq(s.to_lowercase()))
}

pub const RESERVED_CHARS: &[char] = &[
    '+', '-', '*', '/', '%', '"', '<', '>', '!', '=', ';', ',', '(', ')', '[', ']',
];

pub fn is_ident_char(c: char) -> bool {
    !c.is_whitespace() && !RESERVED_CHARS.contains(&c)
}
