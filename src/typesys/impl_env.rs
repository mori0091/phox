use indexmap::IndexMap;

use crate::syntax::ast::*;
use crate::module::*;
use super::*;

// ===== Impl Environment =====
// set of typed impls.
pub struct ImplEnv {
    map: IndexMap<String, SchemeTemplate<TypedImpl>>,
    method_map: IndexMap<Symbol, Vec<SchemeTemplate<Type>>>,
}

impl ImplEnv {
    pub fn new() -> Self {
        ImplEnv {
            map: IndexMap::new(),
            method_map: IndexMap::new(),
        }
    }
    pub fn insert(&mut self, tmpl: SchemeTemplate<TypedImpl>) -> bool {
        let key = tmpl.scheme_ref().pretty();
        if self.map.contains_key(&key) {
            return false;
        }
        for (sym, _e, _ty) in tmpl.scheme_ref().target.members.iter() {
            let member_sch = tmpl.scheme_ref().get_member_scheme(sym).unwrap();
            let member_sch_tmpl = SchemeTemplate::new(member_sch);
            self.method_map.entry(sym.clone()).or_insert(vec![]);
            self.method_map.entry(sym.clone()).and_modify(|xs| xs.push(member_sch_tmpl));
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
    pub fn get_by_head(&self, head: &TraitHead) -> Vec<SchemeTemplate<TypedImpl>> {
        let mut tmpls = Vec::new();
        for tmpl in self.iter() {
            if tmpl.scheme_ref().target.head.pretty() == head.pretty() {
                tmpls.push(tmpl.clone());
            }
        }
        tmpls
    }
    pub fn get_by_member_name(&self, member_name: &Symbol) -> Vec<SchemeTemplate<Type>> {
        match self.method_map.get(member_name) {
            Some(xs) => xs.to_vec(),
            None => vec![],
        }
    }
}
