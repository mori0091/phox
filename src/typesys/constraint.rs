use std::fmt;
use indexmap::IndexSet;

use crate::module::*;
use super::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Constraint {
    Ty(TypeConstraint),
    // Row(RowConstraint),
    // Nat(NatConstraint),
}

impl Constraint {
    pub fn type_eq(t1: &Type, t2: &Type) -> Self {
        Self::Ty(TypeConstraint::TypeEq(t1.clone(), t2.clone()))
    }
    pub fn trait_bound(head: &TraitHead) -> Self {
        Self::Ty(TypeConstraint::TraitBound(head.clone()))
    }
    pub fn overloaded(name: &Symbol, ty: &Type, tmpls: &Vec<SchemeTemplate<Type>>) -> Self {
        Self::Ty(TypeConstraint::Overloaded(name.clone(), ty.clone(), tmpls.clone()))
    }
}

impl FreeVars for Constraint {
    fn free_vars(&self, ctx: &mut UnifiedContext, acc: &mut IndexSet<Var>) {
        match self {
            Self::Ty(c) => c.free_vars(ctx, acc)
        }
    }
}

impl ApplySubst for Constraint {
    fn apply_subst(&self, subst: &Subst) -> Self {
        match self {
            Self::Ty(c) => Self::Ty(c.apply_subst(subst))
        }
    }
}

impl RenameForPretty for Constraint {
    fn rename_var(&self, map: &mut VarNameMap) -> Self {
        match self {
            Self::Ty(c) => Self::Ty(c.rename_var(map))
        }
    }
}

impl Repr for Constraint {
    fn repr(&self, ctx: &mut TypeContext) -> Self {
        match self {
            Self::Ty(c) => Self::Ty(c.repr(ctx))
        }
    }
}

impl Pretty for Constraint {
    fn pretty(&self) -> String {
        match self {
            Self::Ty(c) => c.pretty()
        }
    }
}

impl fmt::Display for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ty(c) => write!(f, "{}", c)
        }
    }
}
