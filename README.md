# novelint

## Syntax

### Instructions
- `print {String|Expr}[,{String|Expr}]*;` : print `String` or/and `Expr`
- `call Ident;` : call a subroutine named `Ident`
- `sub Ident;` : declare a subroutine named `Ident`
- `while CompExpr;` : loop while `CompExpr` is satisfied
- `end;` : ends anything started (e.g. `sub` and `while`)

## TODO
- ~Implement call stack~
    - ~Add `call`~
    - ~Add `while` (implement using call stack?)~
    - Add `break`
    - Add `for`?
- Add arithmetic and comparison
    - Add `if`
    - Add `else`
    - Add `elif`?
- ~Add variable (integer only?)~
    - ~Add `let`
    - ~Add `modify` (modify the value of variable)~
    - Add `freeze` (make the variable immutable)
    - Add lateinit variable?
