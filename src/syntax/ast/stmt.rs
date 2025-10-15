use super::{Expr, Pat};

#[derive(Clone, Debug)]
pub enum Stmt {
    Let(Pat, Box<Expr>),        // `let p = e;`
    LetRec(Pat, Box<Expr>),     // `let rec p = e;`
}
