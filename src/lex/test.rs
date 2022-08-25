#![allow(clippy::too_many_lines)]

use super::*;
use Command::*;
use Keyword::*;
use LangItem::*;

#[test]
fn test_smoke() {
    let s = &[
        "Let light be 0; # This is required for human",
        "let there be light;",
        "print \"hello world\", 1, there;",
    ];

    let expected = Lexed {
        tokens: vec![
            Token {
                loc: Location { row: 1, col: 1 },
                item: Cmd(Let),
            },
            Token {
                loc: Location { row: 1, col: 5 },
                item: Ident("light".into()),
            },
            Token {
                loc: Location { row: 1, col: 11 },
                item: Key(Be),
            },
            Token {
                loc: Location { row: 1, col: 14 },
                item: Num(0, 1),
            },
            Token {
                loc: Location { row: 1, col: 15 },
                item: Semi,
            },
            Token {
                loc: Location { row: 2, col: 1 },
                item: Cmd(Let),
            },
            Token {
                loc: Location { row: 2, col: 5 },
                item: Ident("there".into()),
            },
            Token {
                loc: Location { row: 2, col: 11 },
                item: Key(Be),
            },
            Token {
                loc: Location { row: 2, col: 14 },
                item: Ident("light".into()),
            },
            Token {
                loc: Location { row: 2, col: 19 },
                item: Semi,
            },
            Token {
                loc: Location { row: 3, col: 1 },
                item: Cmd(Print),
            },
            Token {
                loc: Location { row: 3, col: 7 },
                item: Str("hello world".into()),
            },
            Token {
                loc: Location { row: 3, col: 20 },
                item: Comma,
            },
            Token {
                loc: Location { row: 3, col: 22 },
                item: Num(1, 1),
            },
            Token {
                loc: Location { row: 3, col: 23 },
                item: Comma,
            },
            Token {
                loc: Location { row: 3, col: 25 },
                item: Ident("there".into()),
            },
            Token {
                loc: Location { row: 3, col: 30 },
                item: Semi,
            },
        ],
    };

    let actual = lex(s).unwrap();

    pretty_assertions::assert_eq!(expected, actual);
}
