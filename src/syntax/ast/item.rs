use super::{Stmt, Expr, RawTypeDecl};
use super::{RawTraitDecl, RawImplDecl};

#[derive(Clone, Debug)]
pub enum Item {
    RawTraitDecl(RawTraitDecl), // parsed "raw" trait decl.
    RawImplDecl(RawImplDecl),   // parsed "raw" impl decl.
    RawTypeDecl(RawTypeDecl),   // parsed "raw" type decl.
    Stmt(Stmt),
    Expr(Expr),
}

use std::fmt;

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Item::RawTraitDecl(t) => write!(f, "{:?}", t),
            Item::RawImplDecl(t) => write!(f, "{:?}", t),
            Item::RawTypeDecl(t) => write!(f, "{:?}", t),
            Item::Stmt(s) => write!(f, "{}", s),
            Item::Expr(e) => write!(f, "{}", e),
        }
    }
}
