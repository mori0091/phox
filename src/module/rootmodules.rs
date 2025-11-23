use std::collections::HashMap;
use super::*;

pub struct RootModules {
    map: HashMap<String, RefModule>,
}

impl RootModules {
    pub fn new() -> Self {
        RootModules { map: HashMap::new() }
    }

    pub fn add(&mut self, module: RefModule) {
        let name = module.borrow().name.clone();
        self.map.insert(name, module);
    }

    pub fn get(&self, name: &str) -> Option<RefModule> {
        self.map.get(name).cloned()
    }

    pub fn keys(&self) -> std::collections::hash_map::Keys<'_, String, RefModule> {
        self.map.keys()
    }

    pub fn values(&self) -> std::collections::hash_map::Values<'_, String, RefModule> {
        self.map.values()
    }

    pub fn iter(&self) -> std::collections::hash_map::Iter<'_, String, RefModule> {
        self.map.iter()
    }
}
