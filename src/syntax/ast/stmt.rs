use indexmap::IndexSet;
use std::fmt;

use crate::typesys::*;
use super::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Stmt {
    Let(Pat, Box<Expr>),        // `let p = e;`
    LetRec(Pat, Box<Expr>),     // `let rec p = e;`
}

// ----------------------------------------------
// FreeTypeVars
impl FreeVars for Stmt {
    fn free_vars(&self, ctx: &mut UnifiedContext, acc: &mut IndexSet<Var>) {
        match self {
            Stmt::Let(_pat, expr) => {
                expr.free_vars(ctx, acc);
            }
            Stmt::LetRec(_pat, expr) => {
                expr.free_vars(ctx, acc);
            }
        }
    }
}

// ----------------------------------------------
// Repr
impl Repr for Stmt {
    fn repr(&self, ctx: &mut TypeContext) -> Self {
        match self {
            Stmt::Let(pat, expr) => {
                let pat = pat.clone();
                let expr = expr.repr(ctx);
                Stmt::Let(pat, Box::new(expr))
            }
            Stmt::LetRec(pat, expr) => {
                let pat = pat.clone();
                let expr = expr.repr(ctx);
                Stmt::LetRec(pat, Box::new(expr))
            }
        }
    }
}

// ----------------------------------------------
// ApplySubst
impl ApplySubst for Stmt {
    fn apply_subst(&self, subst: &Subst) -> Self {
        match self {
            Stmt::Let(pat, expr) => {
                let pat = pat.clone();
                let expr = expr.apply_subst(subst);
                Stmt::Let(pat, Box::new(expr))
            }
            Stmt::LetRec(pat, expr) => {
                let pat = pat.clone();
                let expr = expr.apply_subst(subst);
                Stmt::LetRec(pat, Box::new(expr))
            }
        }
    }
}

// ----------------------------------------------
// SchemePretty
impl RenameForPretty for Stmt {
    fn rename_var(&self, map: &mut VarNameMap) -> Self {
        match self {
            Stmt::Let(pat, expr) => {
                let pat = pat.clone();
                let expr = expr.rename_var(map);
                Stmt::Let(pat, Box::new(expr))
            }
            Stmt::LetRec(pat, expr) => {
                let pat = pat.clone();
                let expr = expr.rename_var(map);
                Stmt::LetRec(pat, Box::new(expr))
            }
        }
    }
}

// ----------------------------------------------
// Pretty

// ----------------------------------------------
// fmt::Display
impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Let(p, e)    => write!(f, "let {} = {}", p, e),
            Stmt::LetRec(p, e) => write!(f, "let rec {} = {}", p, e),
        }
    }
}
