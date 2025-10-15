use super::{Expr, Pat};

#[derive(Clone, Debug)]
pub enum Stmt {
    Let(Pat, Box<Expr>),        // `let p = e;`
    LetRec(Pat, Box<Expr>),     // `let rec p = e;`
}

use std::fmt;

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Let(p, e)    => write!(f, "let {} = {}", p, e),
            Stmt::LetRec(p, e) => write!(f, "let rec {} = {}", p, e),
        }
    }
}
