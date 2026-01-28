use indexmap::IndexSet;
use super::*;

pub trait FreeVars {
    fn free_vars(&self, ctx: &mut UnifiedContext, acc: &mut IndexSet<Var>);
}
