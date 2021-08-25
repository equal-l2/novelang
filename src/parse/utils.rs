use super::expr_type::{TypeCheck, TypeError};
use super::exprs::TryFromTokens;
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
    ($msg: expr, $($pat: pat)|+, $i: expr, $lexed: ident) => {
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
        ParseError::UnexpectedToken(tk) => {
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
                    format!("Binary operator is not defined for {} and {}", l, r),
                    i,
                    lexed
                );
            }
            TypeError::Unexpected { expected, actual } => {
                die_cont!(format!("Expected {}, found {}", expected, actual), i, lexed);
            }
            TypeError::ArrayTypeDiffer(ty) => {
                die_cont!(
                    format!("Unexpected element in array typed {}", ty),
                    i,
                    lexed
                );
            }
        },
    }
}

// parse tokens into expression
// parse_expr!(i, tks, lexed)
pub(super) fn parse_expr(i: &mut usize, lexed: &lex::Lexed, stack: &ScopeStack) -> Expr {
    let tks = &lexed.tokens;
    let (advanced, expr) = parse_expr_from_tokens(&tks[*i..], stack)
        .unwrap_or_else(|e| die_by_expr_parse_error(e, *i, lexed));
    *i += advanced;
    expr
}

// helper function for parse_expr!
pub(super) fn parse_expr_from_tokens(
    tks: &[lex::Token],
    stack: &ScopeStack,
) -> Result<(usize, Expr), ParseError> {
    if tks.is_empty() {
        return Err(ParseError::EmptyExpr);
    }

    let len = tks.len();

    let mut it = tks.iter().enumerate().peekable();

    let expr = Expr::try_from_tokens(&mut it)?;
    let advanced = it.next().map_or(len, |e| e.0);

    let _ = expr.check_type(stack)?;

    Ok((advanced, expr))
}

macro_rules! parse_stmt {
    ($i: ident, $stmts: ident, $proc: block) => {{
        $i += 1;
        let inst_obj = $proc;
        $stmts.push(inst_obj);
    }};
}

macro_rules! get_type {
    ($expr: ident, $stack: ident, $i: expr, $lexed: ident) => {
        match $expr.check_type(&$stack) {
            Ok(t) => t,
            Err(e) => die_by_expr_parse_error(e.into(), $i, &$lexed),
        }
    };
}