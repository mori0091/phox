use super::{Stmt, Expr};

#[derive(Clone, Debug)]
pub enum Item {
    Stmt(Stmt),
    Expr(Expr),
}

use std::fmt;

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Item::Stmt(s) => write!(f, "{}", s),
            Item::Expr(e) => write!(f, "{}", e),
        }
    }
}
