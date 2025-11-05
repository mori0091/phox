use super::{Expr, Pat};
use crate::module::PathGlob;

#[derive(Clone, Debug)]
pub enum Stmt {
    Mod(String),                // `mod bar;`
    Use(PathGlob),              // `use ::foo::bar;`
    Let(Pat, Box<Expr>),        // `let p = e;`
    LetRec(Pat, Box<Expr>),     // `let rec p = e;`
}

use std::fmt;

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Mod(m)       => write!(f, "mod {}", m),
            Stmt::Use(p)       => write!(f, "use {}", p),
            Stmt::Let(p, e)    => write!(f, "let {} = {}", p, e),
            Stmt::LetRec(p, e) => write!(f, "let rec {} = {}", p, e),
        }
    }
}
