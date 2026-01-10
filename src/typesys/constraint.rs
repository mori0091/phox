use std::fmt;
use std::collections::HashMap;
use std::collections::HashSet;

use crate::module::*;
use super::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Constraint {
    TraitBound(TraitHead),
    TypeEq(Type, Type),
    // ----
    Overloaded(Symbol, Type, Vec<SchemeTemplate<Type>>),
}

impl Constraint {
    pub fn is_generalize_safe(&self) -> bool {
        match self {
            Self::TraitBound(_)       => false,
            Self::TypeEq(_, _)        => true,
            Self::Overloaded(_, _, _) => false,
        }
    }
}

impl Constraint {
    pub fn trait_bound(head: &TraitHead) -> Self {
        Constraint::TraitBound(head.clone())
    }
    pub fn type_eq(t1: &Type, t2: &Type) -> Self {
        Constraint::TypeEq(t1.clone(), t2.clone())
    }
}

// ----------------------------------------------
// FreeTypeVars
impl FreeTypeVars for Constraint {
    fn free_type_vars(&self, ctx: &mut TypeContext, acc: &mut HashSet<TypeVarId>) {
        match self {
            Self::TraitBound(head) => {
                head.free_type_vars(ctx, acc);
            }
            Self::TypeEq(t1, t2) => {
                t1.free_type_vars(ctx, acc);
                t2.free_type_vars(ctx, acc);
            }
            Self::Overloaded(_sym, ty, _sch_tmpls) => {
                ty.free_type_vars(ctx, acc);
                // \NOTE Don't touch _sch_tmpls: Vec<SchemeTemplate<Type>>
            }
        }
    }
}

// ----------------------------------------------
// Repr
impl Repr for Constraint {
    fn repr(&self, ctx: &mut TypeContext) -> Self {
        match self {
            Self::TraitBound(head) => {
                Self::TraitBound(head.repr(ctx))
            }
            Self::TypeEq(t1, t2) => {
                Self::TypeEq(t1.repr(ctx), t2.repr(ctx))
            }
            Self::Overloaded(sym, ty, sch_tmpls) => {
                Self::Overloaded(sym.clone(), ty.repr(ctx), sch_tmpls.clone())
            }
        }
    }
}

// ----------------------------------------------
// ApplySubst
impl ApplySubst for Constraint {
    fn apply_subst(&self, subst: &Subst) -> Self {
        match self {
            Self::TraitBound(head) => {
                let head = head.apply_subst(subst);
                Self::TraitBound(head)
            },
            Self::TypeEq(t1, t2) => {
                let t1 = t1.apply_subst(subst);
                let t2 = t2.apply_subst(subst);
                Self::TypeEq(t1, t2)
            },
            Self::Overloaded(sym, ty, sch_tmpls) => {
                let sym = sym.clone();
                let ty = ty.apply_subst(subst);
                let sch_tmpls = sch_tmpls.clone();
                Self::Overloaded(sym, ty, sch_tmpls)
            },
        }
    }
}

// ----------------------------------------------
// SchemePretty
impl SchemePretty for Constraint {
    fn rename_type_var(&self, map: &mut HashMap<TypeVarId, String>) -> Self {
        match self {
            Self::TraitBound(head) => {
                let head = head.rename_type_var(map);
                Self::TraitBound(head)
            }
            Self::TypeEq(t1, t2) => {
                let t1 = t1.rename_type_var(map);
                let t2 = t2.rename_type_var(map);
                Self::TypeEq(t1, t2)
            },
            Self::Overloaded(sym, ty, sch_tmpls) => {
                let sym = sym.clone();
                let ty = ty.rename_type_var(map);
                let sch_tmpls = sch_tmpls.clone();
                Self::Overloaded(sym, ty, sch_tmpls)
            },
        }
    }
}


// ----------------------------------------------
// Pretty
impl Pretty for Constraint {
    fn pretty(&self) -> String {
        match self {
            Self::TraitBound(head) => {
                format!("{}", head.pretty())
            }
            Self::TypeEq(t1, t2) => {
                format!("{} == {}", t1.pretty(), t2.pretty())
            },
            Self::Overloaded(sym, _ty, _sch_tmpls) => {
                format!("<overloaded {}>", sym.pretty())
            },
        }
    }
}


// ----------------------------------------------
// fmt::Display
impl fmt::Display for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TraitBound(head) => {
                write!(f, "{}", head)
            }
            Self::TypeEq(t1, t2) => {
                write!(f, "{} == {}", t1, t2)
            },
            Self::Overloaded(sym, _ty, _sch_tmpls) => {
                write!(f, "<overloaded {}>", sym.pretty())
            },
        }
    }
}
