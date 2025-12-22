use std::collections::HashMap;
use super::{TypeVarId, Type};

pub type Subst = HashMap<TypeVarId, Type>;

pub trait ApplySubst {
    fn apply_subst(&self, subst: &Subst) -> Self;
}
