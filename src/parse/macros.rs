use super::exprs::TryFromTokens;
use super::type_check::{TypeCheck, TypeError};
use super::ParseError;
use super::ScopeStack;
pub use crate::die;
use crate::exprs::Expr;
use crate::lex;

// die with context (show error message using loc info)
macro_rules! die_cont {
    ($msg: expr, $i: expr, $lexed: ident) => {
        die!(
            "Error: {}\n{}",
            $msg,
            $lexed.generate_loc_info(&$lexed.tokens[$i].loc)
        )
    };
}

// expects!("message here", SomeItem | AnotherItem, i, lexed);
macro_rules! expects {
    ($msg: expr, $($pat: pat)|+, $i: ident, $lexed: ident) => {
        {
            if $lexed.tokens.len() <= $i {
                // tokens has been exhausted
                let last_token = &$lexed.tokens.last().unwrap();
                die!(
                    "Error: {}\n{}",
                    $msg,
                    $lexed.generate_loc_info(&last_token.next_col_loc())
                );
            } else if !matches!(&$lexed.tokens[$i].item, $($pat)|+) {
                die_cont!($msg, $i, $lexed);
            }
            $i += 1;
        }
    }
}

// expects_semi!(i, lexed);
macro_rules! expects_semi {
    ($i: ident, $lexed: ident) => {
        expects!("Semicolon expected", Items::Semi, $i, $lexed);
    };
}

pub(super) fn die_by_expr_parse_error(e: ParseError, i: usize, lexed: &lex::Lexed) -> ! {
    match e {
        ParseError::EmptyExpr => {
            die_cont!("Expr is empty", i, lexed);
        }
        ParseError::InvalidToken(tk) => {
            die!(
                "Error: {}\n{}",
                "Failed to parse expr because of this token",
                lexed.generate_loc_info(&tk.loc)
            );
        }
        ParseError::NoPairParen { lparen: tk } => {
            die!(
                "Error: {}\n{}",
                "Paren doesn't have its pair",
                lexed.generate_loc_info(&tk.loc)
            );
        }
        ParseError::TrailingToken { from: tk } => {
            die!(
                "Error: {}\n{}",
                "Trailing token from here",
                lexed.generate_loc_info(&tk.loc)
            );
        }
        ParseError::TokenExhausted => {
            die_cont!("Expression abruptly ended", i, lexed);
        }
        ParseError::TypeError(te) => match te {
            TypeError::VarNotFound(name) => {
                die_cont!(format!("Variable {} was not found", name), i, lexed);
            }
            TypeError::UnaryUndefined(ty) => {
                //TODO: show operator (such as '<=')
                die_cont!(
                    format!("Unary operator is not defined for {}", ty),
                    i,
                    lexed
                );
            }
            TypeError::BinaryUndefined(l, r) => {
                //TODO: show operator (such as '-' or '+')
                die_cont!(
                    format!("Unary operator is not defined for {} and {}", l, r),
                    i,
                    lexed
                );
            }
        },
    }
}

// parse tokens into expression
// parse_expr!(EndItemOfExpr | AnotherEndOfExpr, i, tks, lexed)
macro_rules! parse_expr {
    ($($end_pat: pat)|+, $i: ident, $tks: ident, $lexed: ident, $stack: ident) => {
        {
            let mut j = $i;
            while j < $tks.len()
                && !matches!(
                    $tks[j].item,
                    $($end_pat)|+
                )
            {
                j += 1;
            }
            let expr = parse_expr_from_tokens(&$tks[$i..j], &$stack).unwrap_or_else(
                |e| die_by_expr_parse_error(e, $i, &$lexed)
            );
            $i = j;
            expr
        }
    }
}

// helper function for parse_expr!
pub(super) fn parse_expr_from_tokens(
    tks: &[lex::Token],
    stack: &ScopeStack,
) -> Result<Expr, ParseError> {
    if tks.is_empty() {
        return Err(ParseError::EmptyExpr);
    }

    let expr = Expr::try_from_tokens(&mut tks.iter().peekable())?;

    let _ = expr.check_type(stack)?;

    Ok(expr)
}

macro_rules! parse_stmt {
    ($i: ident, $stmts: ident, $proc: block) => {{
        $i += 1;
        let inst_obj = $proc;
        $stmts.push(inst_obj);
    }};
}

macro_rules! expects_type {
    ($expr: ident, $ty: path, $stack: ident, $i: ident, $lexed: ident) => {
        match $expr.check_type(&$stack) {
            Ok(t) => {
                if t != $ty {
                    die_cont!(format!("Expected {}, found {}", $ty, t), $i, $lexed)
                }
            }
            Err(e) => {
                die_by_expr_parse_error(e.into(), $i, &$lexed);
            }
        }
    };
}
