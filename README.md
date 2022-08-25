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
<call>     ::= "call" EXPR ";"
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
<sub>      ::= "sub" IDENT ";"
<end>      ::= "end" ";"
<return>   ::= "return" ";"
```

## String arithmetic
- `-<str>` : string inversion
- `<str> + <str>` : string concatenation
- `<str> * <num>` or `<num> * <str>` : repeating string

## TODO
- add subroutine with return value
- add regex matching?
