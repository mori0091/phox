use std::collections::HashMap;
use crate::syntax::ast::*;
use crate::module::*;
use super::*;

// ===== Impl Environment =====
// set of typed impls.
pub struct ImplEnv {
    map: HashMap<String, SchemeTemplate<TypedImpl>>,
}

impl ImplEnv {
    pub fn new() -> Self {
        ImplEnv {
            map: HashMap::new(),
        }
    }
    pub fn insert(&mut self, tmpl: SchemeTemplate<TypedImpl>) -> bool {
        let key = tmpl.scheme_ref().pretty();
        if self.map.contains_key(&key) {
            return false;
        }
        self.map.insert(key, tmpl);
        true
    }
    pub fn iter(&self) -> impl Iterator<Item = &SchemeTemplate<TypedImpl>> {
        self.map.values()
    }
    pub fn get_by_name(&self, trait_name: &Symbol) -> Vec<SchemeTemplate<TypedImpl>> {
        let mut tmpls = Vec::new();
        for tmpl in self.iter() {
            if tmpl.scheme_ref().target.head.name == *trait_name {
                tmpls.push(tmpl.clone());
            }
        }
        tmpls
    }
}
