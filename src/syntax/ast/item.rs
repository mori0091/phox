use super::{Stmt, Expr, RawTypeDecl};

#[derive(Clone, Debug)]
pub enum Item {
    RawTypeDecl(RawTypeDecl),   // parsed "raw" type decl.
    Stmt(Stmt),
    Expr(Expr),
}

use std::fmt;

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Item::RawTypeDecl(t) => write!(f, "{:?}", t),
            Item::Stmt(s) => write!(f, "{}", s),
            Item::Expr(e) => write!(f, "{}", e),
        }
    }
}
