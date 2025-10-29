use std::collections::HashMap;
use super::{TypeVarId, Type};

pub trait ApplySubst {
    fn apply_subst(&self, subst: &HashMap<TypeVarId, Type>) -> Self;
}
