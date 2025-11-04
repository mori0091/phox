use std::collections::HashMap;
use super::{Module, RefModule};

pub struct RootModules {
    map: HashMap<String, RefModule>,
}

impl RootModules {
    pub fn new() -> Self {
        RootModules { map: HashMap::new() }
    }

    pub fn add(&mut self, name: String, module: RefModule) {
        self.map.insert(name, module);
    }

    pub fn get(&self, name: &str) -> Option<RefModule> {
        self.map.get(name).cloned()
    }

    pub fn get_mut(&self, name: &str) -> Option<std::cell::RefMut<'_, Module>> {
        self.map.get(name).map(|m| m.borrow_mut())
    }
}
