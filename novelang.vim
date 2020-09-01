syntax case ignore
syntax keyword Statement Print Sub Call While Let Modify Input If ElIf Else End
syntax keyword Statement Roll Halt Break Be To AsMut EnableWait DisableWait
syntax match Comment "^\s*#.*$"
syntax region String start="\"" end="\""
