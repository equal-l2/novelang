if exists("b:did_indent")
    finish
endif
let b:did_indent = 1

setlocal indentexpr=GetNovelangIndent(v:lnum)

set indentkeys=!^F,o,O,=else,=end

if exists("*GetNovelangIndent")
    finish
endif

function! GetNovelangIndent(...)
    let l:plnum = prevnonblank(v:lnum-1)
    let l:previndent = indent(plnum)
    let l:prevline = getline(plnum)
    let l:indentreg = '\v^\s*(sub|if|else|while)'
    let l:dedentreg = '\v^\s*(else|end)'

    if getline(v:lnum) =~? l:dedentreg
        if l:prevline =~? l:indentreg
            # when prev body is empty
            let l:res = l:previndent
        else
            let l:res = l:previndent - &l:shiftwidth
        endif
    elseif l:prevline =~? l:indentreg
        let l:res = l:previndent + &l:shiftwidth
    else
        let l:res = l:previndent
    endif

    #echom l:res

    return l:res
endfunction
