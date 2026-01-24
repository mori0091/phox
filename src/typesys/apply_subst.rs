use std::collections::HashMap;
use super::*;

pub type Subst = HashMap<Var, Type>;

pub trait ApplySubst {
    fn apply_subst(&self, subst: &Subst) -> Self;
}
