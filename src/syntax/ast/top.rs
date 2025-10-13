use super::{Expr, RawTypeDecl};

pub type Program = Vec<TopLevel>;

pub enum TopLevel {
    TypeDecl(RawTypeDecl),
    Expr(Expr),
}
