use indexmap::IndexMap;

use crate::syntax::ast::*;
use crate::module::*;
use super::*;

// ===== Starlet Environment =====
// set of typed starlets.
pub struct StarletEnv {
    map: IndexMap<String, SchemeTemplate<TypedStarlet>>,
}

impl StarletEnv {
    pub fn new() -> Self {
        StarletEnv {
            map: IndexMap::new(),
        }
    }
    pub fn insert(&mut self, tmpl: SchemeTemplate<TypedStarlet>) -> bool {
        let key = tmpl.scheme_ref().pretty();
        if self.map.contains_key(&key) {
            return false;
        }
        self.map.insert(key, tmpl);
        true
    }
    pub fn iter(&self) -> impl Iterator<Item = &SchemeTemplate<TypedStarlet>> {
        self.map.values()
    }
    pub fn is_starlet(&self, name: &Symbol) -> bool {
        self.iter().any(|tmpl| tmpl.scheme_ref().target.name == *name)
    }
    pub fn get_by_name(&self, name: &Symbol) -> Vec<SchemeTemplate<TypedStarlet>> {
        let mut tmpls = Vec::new();
        for tmpl in self.iter() {
            if tmpl.scheme_ref().target.name == *name {
                tmpls.push(tmpl.clone());
            }
        }
        tmpls
    }
}
