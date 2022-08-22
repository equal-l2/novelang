use super::*;
use Command::*;
use LangItem::*;
use Keyword::*;

#[test]
fn test_smoke() {
    let s = r#"
Let light be 0; # This is required for human
let there be light;
print "hello world", 1, there;
            "#;

    let expected = Lexed {
        lines: s.lines().map(str::to_owned).collect(),
        tokens: vec![
            Token {
                loc: Location { row: 2, col: 1 },
                item: Cmd(Let),
            },
            Token {
                loc: Location { row: 2, col: 5 },
                item: Ident("light".into()),
            },
            Token {
                loc: Location { row: 2, col: 11 },
                item: Key(Be),
            },
            Token {
                loc: Location { row: 2, col: 14 },
                item: Num(0, 1),
            },
            Token {
                loc: Location { row: 2, col: 15 },
                item: Semi,
            },
            Token {
                loc: Location { row: 3, col: 1 },
                item: Cmd(Let),
            },
            Token {
                loc: Location { row: 3, col: 5 },
                item: Ident("there".into()),
            },
            Token {
                loc: Location { row: 3, col: 11 },
                item: Key(Be),
            },
            Token {
                loc: Location { row: 3, col: 14 },
                item: Ident("light".into()),
            },
            Token {
                loc: Location { row: 3, col: 19 },
                item: Semi,
            },
            Token {
                loc: Location { row: 4, col: 1 },
                item: Cmd(Print),
            },
            Token {
                loc: Location { row: 4, col: 7 },
                item: Str("hello world".into()),
            },
            Token {
                loc: Location { row: 4, col: 20 },
                item: Comma,
            },
            Token {
                loc: Location { row: 4, col: 22 },
                item: Num(1, 1),
            },
            Token {
                loc: Location { row: 4, col: 23 },
                item: Comma,
            },
            Token {
                loc: Location { row: 4, col: 25 },
                item: Ident("there".into()),
            },
            Token {
                loc: Location { row: 4, col: 30 },
                item: Semi,
            },
        ],
    };

    let actual = lex(s).unwrap();

    pretty_assertions::assert_eq!(expected, actual);
}

#[test]
fn test_alias() {
    let s = r#"
Roll 1 die with 1 face to _;
Roll 2 dice with 2 faces to _;
Roll 1 die with 2 faces to _;
Roll 2 dice with 1 face to _;
            "#;

    let expected = Lexed {
        lines: s.lines().map(str::to_owned).collect(),
        tokens: vec![
            Token {
                loc: Location { row: 2, col: 1 },
                item: Cmd(Roll),
            },
            Token {
                loc: Location { row: 2, col: 6 },
                item: Num(1, 1),
            },
            Token {
                loc: Location { row: 2, col: 8 },
                item: Key(Dice),
            },
            Token {
                loc: Location { row: 2, col: 12 },
                item: Key(With),
            },
            Token {
                loc: Location { row: 2, col: 17 },
                item: Num(1, 1),
            },
            Token {
                loc: Location { row: 2, col: 19 },
                item: Key(Faces),
            },
            Token {
                loc: Location { row: 2, col: 24 },
                item: Key(To),
            },
            Token {
                loc: Location { row: 2, col: 27 },
                item: Ident("_".into()),
            },
            Token {
                loc: Location { row: 2, col: 28 },
                item: Semi,
            },
            Token {
                loc: Location { row: 3, col: 1 },
                item: Cmd(Roll),
            },
            Token {
                loc: Location { row: 3, col: 6 },
                item: Num(2, 1),
            },
            Token {
                loc: Location { row: 3, col: 8 },
                item: Key(Dice),
            },
            Token {
                loc: Location { row: 3, col: 13 },
                item: Key(With),
            },
            Token {
                loc: Location { row: 3, col: 18 },
                item: Num(2, 1),
            },
            Token {
                loc: Location { row: 3, col: 20 },
                item: Key(Faces),
            },
            Token {
                loc: Location { row: 3, col: 26 },
                item: Key(To),
            },
            Token {
                loc: Location { row: 3, col: 29 },
                item: Ident("_".into()),
            },
            Token {
                loc: Location { row: 3, col: 30 },
                item: Semi,
            },
            Token {
                loc: Location { row: 4, col: 1 },
                item: Cmd(Roll),
            },
            Token {
                loc: Location { row: 4, col: 6 },
                item: Num(1, 1),
            },
            Token {
                loc: Location { row: 4, col: 8 },
                item: Key(Dice),
            },
            Token {
                loc: Location { row: 4, col: 12 },
                item: Key(With),
            },
            Token {
                loc: Location { row: 4, col: 17 },
                item: Num(2, 1),
            },
            Token {
                loc: Location { row: 4, col: 19 },
                item: Key(Faces),
            },
            Token {
                loc: Location { row: 4, col: 25 },
                item: Key(To),
            },
            Token {
                loc: Location { row: 4, col: 28 },
                item: Ident("_".into()),
            },
            Token {
                loc: Location { row: 4, col: 29 },
                item: Semi,
            },
            Token {
                loc: Location { row: 5, col: 1 },
                item: Cmd(Roll),
            },
            Token {
                loc: Location { row: 5, col: 6 },
                item: Num(2, 1),
            },
            Token {
                loc: Location { row: 5, col: 8 },
                item: Key(Dice),
            },
            Token {
                loc: Location { row: 5, col: 13 },
                item: Key(With),
            },
            Token {
                loc: Location { row: 5, col: 18 },
                item: Num(1, 1),
            },
            Token {
                loc: Location { row: 5, col: 20 },
                item: Key(Faces),
            },
            Token {
                loc: Location { row: 5, col: 25 },
                item: Key(To),
            },
            Token {
                loc: Location { row: 5, col: 28 },
                item: Ident("_".into()),
            },
            Token {
                loc: Location { row: 5, col: 29 },
                item: Semi,
            },
        ],
    };

    let actual = lex(s).unwrap();

    pretty_assertions::assert_eq!(expected, actual);
}
