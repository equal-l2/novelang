syntax clear

syntax case match
syntax keyword Special _wait

syntax case ignore
syntax keyword Statement Print Sub Call While Let Modify Input End
syntax keyword Statement Roll Halt Break Be To
syntax keyword Statement Die Dice With Faces Face
syntax keyword Statement Sub Call nextgroup=FuncName skipwhite skipempty
syntax keyword Boolean True False
syntax keyword StorageClass AsMut
syntax keyword Conditional If Else
syntax match Comment '^\s*#.*$' display
syntax match FuncName '[^[:cntrl:][:space:][:digit:][:punct:]]\%([^[:cntrl:][:space:][:punct:]]\|_\)*' contained display
syntax match Operator '+\|-\|*\|/\|%\|<\|>\|=\|!' display
syntax match Number '\<\d*\>' display
syntax region String start='\"' end='\"' display

highlight default link FuncName Function
