use super::exprs::TypeCheck;
use super::ParseError;
pub use crate::die;
use crate::exprs::Expr;

/*
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
*/

pub(super) fn get_type(expr: &Expr, stack: &super::ScopeStack) -> super::types::Type {
    match expr.check_type(&stack) {
        Ok(t) => t,
        Err(e) => die!("Invalid expr: {:?}", ParseError::from(e)), // TODO: impl Display
    }
}
