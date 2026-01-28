use indexmap::IndexSet;
use std::fmt;

use crate::module::*;
use crate::typesys::*;
use super::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Stmt {
    Mod(String, Option<Vec<Item>>), // `mod bar;`
    Use(PathGlob),              // `use ::foo::bar;`
    Let(Pat, Box<Expr>),        // `let p = e;`
    LetRec(Pat, Box<Expr>),     // `let rec p = e;`
}

// ----------------------------------------------
// FreeTypeVars
impl FreeVars for Stmt {
    fn free_vars(&self, ctx: &mut UnifiedContext, acc: &mut IndexSet<Var>) {
        match self {
            Stmt::Mod(_name, opt_itmes) => {
                match opt_itmes {
                    Some(items) => {
                        for item in items {
                            item.free_vars(ctx, acc);
                        }
                    }
                    None => {}
                }
            }
            Stmt::Use(_) => {}
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
            Stmt::Mod(name, opt_itmes) => {
                let name = name.clone();
                let opt_itmes = match opt_itmes {
                    Some(items) => {
                        let items = items.iter().map(|item| item.repr(ctx)).collect();
                        Some(items)
                    }
                    None => None,
                };
                Stmt::Mod(name, opt_itmes)
            }
            Stmt::Use(_) => { self.clone() }
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
            Stmt::Mod(name, opt_itmes) => {
                let name = name.clone();
                let opt_itmes = match opt_itmes {
                    Some(items) => {
                        let items = items.iter().map(|item| item.apply_subst(subst)).collect();
                        Some(items)
                    }
                    None => None,
                };
                Stmt::Mod(name, opt_itmes)
            }
            Stmt::Use(_) => { self.clone() }
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
            Stmt::Mod(name, opt_itmes) => {
                let name = name.clone();
                let opt_itmes = match opt_itmes {
                    Some(items) => {
                        let items = items.iter().map(|item| item.rename_var(map)).collect();
                        Some(items)
                    }
                    None => None,
                };
                Stmt::Mod(name, opt_itmes)
            }
            Stmt::Use(_) => { self.clone() }
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
            Stmt::Mod(m, items) => {
                if let Some(items) = items {
                    let s: Vec<String> = items.iter()
                        .map(|item| format!("  {};\n", item))
                        .collect();
                    write!(f, "mod {} {{\n{}\n}}", m, s.join("\n"))
                }
                else {
                    write!(f, "mod {}", m)
                }
            },
            Stmt::Use(p)       => write!(f, "use {}", p),
            Stmt::Let(p, e)    => write!(f, "let {} = {}", p, e),
            Stmt::LetRec(p, e) => write!(f, "let rec {} = {}", p, e),
        }
    }
}
