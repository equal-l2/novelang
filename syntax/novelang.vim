" Novelang syntax file

if exists("b:current_syntax")
  finish
endif

let b:current_syntax = "novelang"

syntax case match
syntax keyword novelangInternal _wait

syntax case ignore
syntax keyword novelangCommand Assert Call Halt Input Let Modify Print Roll Break Continue End Return
syntax keyword novelangCommand Sub nextgroup=novelangFuncName skipwhite skipempty
syntax keyword novelangKeyword With To Be Dice Faces From In Results
syntax keyword novelangBoolean True False
syntax keyword novelangAsMut AsMut
syntax keyword novelangConditional If Else
syntax keyword novelangRepeat For While
syntax keyword novelangType Bool Num Str
syntax match novelangIdentifier '\<.\{-}\>' display " this must be the top of matches because this can be override all other matches unless this has the lowest priority
syntax match novelangComment '#.*$' display
syntax match novelangFuncName '[^[:cntrl:][:space:][:digit:][:punct:]]\%([^[:cntrl:][:space:][:punct:]]\|_\)*' contained display
syntax match novelangOperator ';\|&&\|||\|==\|/=\|<=\|>=\|<\|>\|+\|-\|*\|/\|%\|\[\|\]\|(\|)' display
syntax match novelangDelimiter ',' display
syntax match novelangNumber '\<\d*\>' display
syntax region novelangString start='\"' end='\"' display

highlight default link novelangInternal Special
highlight default link novelangCommand Keyword
highlight default link novelangKeyword Keyword
highlight default link novelangBoolean Boolean
highlight default link novelangAsMut StorageClass
highlight default link novelangConditional Conditional
highlight default link novelangRepeat Repeat
highlight default link novelangType Type
highlight default link novelangIdentifier Identifier
highlight default link novelangComment Comment
highlight default link novelangFuncName Function
highlight default link novelangOperator Operator
highlight default link novelangDelimiter Delimiter
highlight default link novelangNumber Number
highlight default link novelangString String
