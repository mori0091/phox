use indexmap::IndexSet;

use super::*;

#[derive(Clone)]
pub struct UnifiedContext {
    pub non_touchable: IndexSet<Var>,
    pub ty: TypeContext,
    // pub row: RowContext,
    // pub nat: NatContext,
}
impl UnifiedContext {
    pub fn new() -> Self {
        Self {
            non_touchable: IndexSet::new(),
            ty: TypeContext::new(),
        }
    }

    pub fn set_non_touchable(&mut self, non_touchable: &IndexSet<Var>) {
        self.non_touchable.extend(non_touchable);
        self.ty.non_touchable.extend(non_touchable.iter().filter_map(|v| {
            match v {
                Var::Ty(id) => Some(id),
                // _ => None,
            }
        }));
    }

    pub fn clear_non_touchable(&mut self) {
        self.non_touchable.clear();
        self.ty.non_touchable.clear();
    }
}
