# novelang

> "<code><b>Let</b> there <b>be</b> light;</code>" That's, uh... God. I was quoting God.

## Syntax

```
<program>    ::= {<line>}
<line>       ::= {<stmt>} ["#" { CHAR }] NL
<stmt>       ::= <print> | <sub> | <call> | <while> | <let>
               | <modify> | <input> | <if> | <elif> | <else>
               | <end> | <roll> | <halt> | <break>
<print>      ::= "print" <print-args> ";"
<print-args> ::= (<expr> | <string>) {"," (<expr> | <string>)}
<sub>        ::= "sub" IDENT ";"
<call>       ::= "call" IDENT ";"
<while>      ::= "while" <expr> ";"
<let>        ::= "let" IDENT "be" <expr> ["asmut"] ";"
<modify>     ::= "modify" IDENT "to" <expr> ";"
<input>      ::= "input" [<string>] "to" <ident>";"
<if>         ::= "if" <expr> ";"
<elif>       ::= "else" "if" <expr> ";"
<else>       ::= "else" ";"
<end>        ::= "end" ";"
<roll>       ::= "roll" <expr> ("die"|"dice") "with" <expr> ("face"|"faces") "to" <ident>";"
<halt>       ::= "halt" ";"
<break>      ::= "break" ";"
```

## Instructions
- `print {String|Expr}[,{String|Expr}]*;` : print `String` or/and `Expr`.
- `sub Ident;` : declare a subroutine named `Ident`.
- `call Ident;` : call a subroutine named `Ident`.
- `while CompExpr;` : loop while `CompExpr` is satisfied.
- `let Ident be InitExpr [asmut];` : declare an ident named `Ident` using the value of `InitExpr`.
- `modify Ident to Expr;` : modify the value of an ident named `Ident` to the value of `InitExpr`.
- `input [prompt];` : get an input to `_result`.
- `if expr / else if expr / else`
- `end;` : ends anything started (e.g. `sub` and `while`)
- `roll Expr1 dice with Expr2 faces;`
- `halt` : halt execution.
- `break`

## String arithmetic
- `-<str>` : string inversion
- `<str> + <str>` : string concatenation
- `<str> * <num>` or `<num> * <str>` : repeating string

## TODO
- Add `for`
- Add array type
- Add compound conditions
    - Add `&&`
    - Add `||`
- add subroutine with return value
- better type checking (i.e. better diagnostics)
