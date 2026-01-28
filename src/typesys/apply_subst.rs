use indexmap::IndexMap;

use super::*;

pub type Subst = IndexMap<Var, Type>;

pub trait ApplySubst {
    fn apply_subst(&self, subst: &Subst) -> Self;
}
