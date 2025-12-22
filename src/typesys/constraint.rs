use std::fmt;
use std::collections::HashMap;
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
    pub fn trait_bound(head: &TraitHead) -> Self {
        Constraint::TraitBound(head.clone())
    }
    pub fn type_eq(t1: &Type, t2: &Type) -> Self {
        Constraint::TypeEq(t1.clone(), t2.clone())
    }
}

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

impl ApplySubst for Constraint {
    fn apply_subst(&self, subst: &Subst) -> Self {
        match self {
            Self::TraitBound(head) => {
                Self::TraitBound(head.apply_subst(subst))
            },
            Self::TypeEq(t1, t2) => {
                Self::TypeEq(t1.apply_subst(subst), t2.apply_subst(subst))
            },
            Self::Overloaded(_sym, _ty, _sch_tmpls) => {
                todo!()
            },
        }
    }
}

// for Scheme::pretty
impl SchemePretty for Constraint {
    fn rename_type_var(&self, map: &mut HashMap<TypeVarId, String>) -> Self {
        match self {
            Self::TraitBound(head) => {
                Self::TraitBound(head.rename_type_var(map))
            }
            Self::TypeEq(t1, t2) => {
                Self::TypeEq(t1.rename_type_var(map), t2.rename_type_var(map))
            },
            Self::Overloaded(_sym, _ty, _sch_tmpls) => {
                todo!()
            },
        }
    }
}

impl Pretty for Constraint {
    fn pretty(&self) -> String {
        match self {
            Self::TraitBound(head) => {
                format!("{}", head.pretty())
            }
            Self::TypeEq(t1, t2) => {
                format!("{} == {}", t1.pretty(), t2.pretty())
            },
            Self::Overloaded(_sym, _ty, _sch_tmpls) => {
                todo!()
            },
        }
    }
}

impl fmt::Display for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.pretty())
    }
}
