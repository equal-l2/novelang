use super::{
    exprs::{ExprError, TryFromTokens},
    lex, Expr,
};
pub use crate::die;

// expects!("message here", SomeItem | AnotherItem, i, lexed);
macro_rules! expects {
    ($msg: expr, $($pat: pat_param)|+, $tks: ident, $last: ident) => {
        {
            let _ret = $tks.next();
            match _ret {
                Some((_i, _tk)) => {
                    if !matches!(_tk.item, $($pat)|+) {
                        return Err(Error($msg.into(), _i.into()));
                    }
                }
                None => {
                    return Err(Error($msg.into(), $last.into()));
                }
            }
        }
    }
}

// expects_semi!(i, lexed);
macro_rules! expects_semi {
    ($tks: ident, $last: ident) => {
        expects!(
            "Semicolon expected",
            crate::lex::LangItem::Semi,
            $tks,
            $last
        )
    };
}

// parse tokens into expression
// parse_expr!(i, tks, lexed)
pub(super) fn parse_expr<'a, T>(
    tks: &mut std::iter::Peekable<T>,
    last: usize,
) -> Result<Expr, ExprError>
where
    T: Iterator<Item = (usize, &'a lex::Token)>,
{
    Expr::try_from_tokens(tks, last)
}

//macro_rules! parse_stmt {
//    ($stmts: ident, $proc: block) => {{
//        let inst_obj = $proc;
//        $stmts.push(inst_obj);
//    }};
//}

macro_rules! parse_normal {
    ($stmts: ident, $proc: block) => {{
        let inst_obj = $proc;
        $stmts.push(Statement::Normal(inst_obj));
    }};
}

macro_rules! parse_block {
    ($stmts: ident, $idx: ident, $proc: block) => {{
        let inst_obj = $proc;
        $stmts.push(Statement::Block(inst_obj, $idx.into()));
    }};
}
