use std::collections::HashSet;
use crate::syntax::ast::*;
use crate::module::*;
use super::*;

// ===== Impl Environment =====
// set of typed impls.
pub struct ImplEnv {
    impls: HashSet<SchemeTemplate<TypedImpl>>,
}

impl ImplEnv {
    pub fn new() -> Self {
        ImplEnv {
            impls: HashSet::new(),
        }
    }
    pub fn insert(&mut self, tmpl: SchemeTemplate<TypedImpl>) -> bool {
        self.impls.insert(tmpl)
    }
    pub fn iter(&self) -> impl Iterator<Item = &SchemeTemplate<TypedImpl>> {
        self.impls.iter()
    }
    pub fn get_by_name(&self, trait_name: &Symbol) -> Vec<SchemeTemplate<TypedImpl>> {
        let mut tmpls = Vec::new();
        for tmpl in &self.impls {
            if tmpl.scheme_ref().target.head.name == *trait_name {
                tmpls.push(tmpl.clone());
            }
        }
        tmpls
    }
}
