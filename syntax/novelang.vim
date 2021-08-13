" Novelang syntax file

if exists("b:current_syntax")
  finish
endif

let b:current_syntax = "novelang"

syntax case match
syntax keyword Special _wait

syntax case ignore
syntax keyword Statement Print While Let Modify Input If Else End Roll Halt Break Assert
syntax keyword Statement Break Assert For Continue
syntax keyword Statement Be To Die Dice With Face Faces From
syntax keyword Statement Sub Call nextgroup=FuncName skipwhite skipempty
syntax keyword Boolean True False
syntax keyword StorageClass AsMut
syntax keyword Conditional If Else
syntax match Comment '^\s*#.*$' display
syntax match FuncName '[^[:cntrl:][:space:][:digit:][:punct:]]\%([^[:cntrl:][:space:][:punct:]]\|_\)*' contained display
syntax match Operator '&&\|||\|+\|-\|*\|/\|%\|<\|>\|=\|!' display
syntax match Number '\<\d*\>' display
syntax region String start='\"' end='\"' display

highlight default link FuncName Function
