# novelang

> "<code><b>Let</b> there <b>be</b> light;</code>" That's, uh... God. I was quoting God.

## Syntax

```
<program>  ::= {<line>}
<line>     ::= {<stmt>} ["#" { CHAR }] NL
<stmt>     ::= <assert> | <call> | <halt> | <input> | <let> | <modify> | <print> | <roll>
             | <for> | <while> | <break> | <continue> | <if> | <elif> | <else>
             | <sub> | <return> | <end>

<assert>   ::= "assert" [EXPR "with"] EXPR ";"
<call>     ::= "call" EXPR ["with" EXPR {"," EXPR}] ["results" "in" TARGET]";"
<halt>     ::= "halt" ";"
<input>    ::= "input" [STRING] "to" TARGET ";"
<let>      ::= "let" IDENT "be" EXPR ["asmut"] ";"
<modify>   ::= "modify" TARGET "to" EXPR ";"
<print>    ::= "print" EXPR {"," EXPR } ";"
<roll>     ::= "roll" EXPR "dice" "with" EXPR "faces" "to" TARGET ";"
<for>      ::= "for" IDENT "from" EXPR "to" EXPR ";"
<while>    ::= "while" EXPR ";"
<break>    ::= "break" ";"
<continue> ::= "continue" ";"
<if>       ::= "if" EXPR ";"
<elif>     ::= "else" "if" EXPR ";"
<else>     ::= "else" ";"
<sub>      ::= "sub" IDENT ["with" IDENT "in" TYPE {"," IDENT "in" TYPE}] ["results" "in" TARGET]";"
<end>      ::= "end" ";"
<return>   ::= "return" ["with" EXPR] ";"

EXPR      ::= <log>
<log>     ::= <equ> {("&&" | "||") <equ>}
<equ>     ::= <rel> {("==" | "/=") <rel>}
<rel>     ::= <add-sub> {("<=" | ">=" || "<" || ">") <add-sub>}
<add-sub> ::= <mul-div> {("+" | "-") <mul-div>}
<mul-div> ::= <node> {("*" | "/" | "%") <node>}
<node>    ::= {"+" | "-"} <value>
<value>   ::= <core> [ "[" <add-sub> "]" ]
<core>    ::= STRING | NUMBER | IDENT | "true" | "false" | "(" <log> ")" | "[" <log> {"," <log>} "]"

TARGET ::= IDENT {"[" EXPR "]"}
```

## String arithmetic

- `-<str>` : string inversion
- `<str> + <str>` : string concatenation
- `<str> * <num>` or `<num> * <str>` : repeating string

## TODO

- add subroutine with return value
- add regex matching?
