use std::collections::HashSet;
use super::{TypeContext, TypeVarId};

pub trait FreeTypeVars {
    fn free_type_vars(&self, ctx: &mut TypeContext, acc: &mut HashSet<TypeVarId>);
}
