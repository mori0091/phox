use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;

use crate::typesys::*;
use super::{Decl, Stmt, Expr};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Item {
    pub body: ItemBody,
    pub constraints: Vec<Constraint>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ItemBody {
    Decl(Decl),
    Stmt(Stmt),
    Expr(Expr),
}

impl Item {
    pub fn decl(d: Decl) -> Item {
        Item { body: ItemBody::Decl(d), constraints: vec![] }
    }
    pub fn stmt(s: Stmt) -> Item {
        Item { body: ItemBody::Stmt(s), constraints: vec![] }
    }
    pub fn expr(e: Expr) -> Item {
        Item { body: ItemBody::Expr(e), constraints: vec![] }
    }
}

// ----------------------------------------------
// FreeTypeVars
impl FreeTypeVars for Item {
    fn free_type_vars(&self, ctx: &mut TypeContext, acc: &mut HashSet<TypeVarId>) {
        for c in &self.constraints {
            c.free_type_vars(ctx, acc);
        }
        match &self.body {
            ItemBody::Decl(_decl) => {}
            ItemBody::Stmt(stmt) => {
                stmt.free_type_vars(ctx, acc);
            }
            ItemBody::Expr(expr) => {
                expr.free_type_vars(ctx, acc);
            }
        }
    }
}

// ----------------------------------------------
// Repr
impl Repr for Item {
    fn repr(&self, ctx: &mut TypeContext) -> Self {
        let constraints = self.constraints.iter().map(|c| c.repr(ctx)).collect();
        let body = match &self.body {
            ItemBody::Decl(decl) => {
                let decl = decl.clone();
                ItemBody::Decl(decl)
            }
            ItemBody::Stmt(stmt) => {
                let stmt = stmt.repr(ctx);
                ItemBody::Stmt(stmt)
            }
            ItemBody::Expr(expr) => {
                let expr = expr.repr(ctx);
                ItemBody::Expr(expr)
            }
        };
        Item { body, constraints }
    }
}

// ----------------------------------------------
// ApplySubst
impl ApplySubst for Item {
    fn apply_subst(&self, subst: &Subst) -> Self {
        let constraints = self.constraints.iter().map(|c| c.apply_subst(subst)).collect();
        let body = match &self.body {
            ItemBody::Decl(decl) => {
                let decl = decl.clone();
                ItemBody::Decl(decl)
            }
            ItemBody::Stmt(stmt) => {
                let stmt = stmt.apply_subst(subst);
                ItemBody::Stmt(stmt)
            }
            ItemBody::Expr(expr) => {
                let expr = expr.apply_subst(subst);
                ItemBody::Expr(expr)
            }
        };
        Item { body, constraints }
    }
}

// ----------------------------------------------
// SchemePretty
impl SchemePretty for Item {
    fn rename_type_var(&self, map: &mut HashMap<TypeVarId, String>) -> Self {
        let constraints = self.constraints.iter().map(|c| c.rename_type_var(map)).collect();
        let body = match &self.body {
            ItemBody::Decl(decl) => {
                let decl = decl.clone();
                ItemBody::Decl(decl)
            }
            ItemBody::Stmt(stmt) => {
                let stmt = stmt.rename_type_var(map);
                ItemBody::Stmt(stmt)
            }
            ItemBody::Expr(expr) => {
                let expr = expr.rename_type_var(map);
                ItemBody::Expr(expr)
            }
        };
        Item { body, constraints }
    }
}

// ----------------------------------------------
// Pretty

// ----------------------------------------------
// fmt::Display
impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.body)
    }
}

impl fmt::Display for ItemBody {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ItemBody::Decl(d) => write!(f, "{}", d),
            ItemBody::Stmt(s) => write!(f, "{}", s),
            ItemBody::Expr(e) => write!(f, "{}", e),
        }
    }
}
