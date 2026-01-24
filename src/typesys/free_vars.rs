use std::collections::HashSet;
use super::*;

pub trait FreeVars {
    // fn free_vars(&self, ctx: &mut TypeContext, acc: &mut HashSet<TypeVarId>);
    fn free_vars(&self, ctx: &mut TypeContext, acc: &mut HashSet<Var>);
}
