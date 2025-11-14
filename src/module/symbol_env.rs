use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use super::*;

pub type Binding = HashMap<Path, Symbol>;

/// resolve 時の環境
#[derive(Clone)]
pub struct SymbolEnv {
    map: Rc<RefCell<Binding>>,
}

impl SymbolEnv {
    pub fn new() -> Self {
        SymbolEnv { map: Rc::new(RefCell::new(HashMap::new())) }
    }

    pub fn insert(&self, k: Path, v: Symbol) {
        self.map.borrow_mut().insert(k, v);
    }

    pub fn get(&self, k: &Path) -> Option<Symbol> {
        self.map.borrow().get(k).cloned()
    }

    pub fn extend(&self, other: &Binding) {
        self.map.borrow_mut().extend(other.clone());
    }

    pub fn clone_map(&self) -> Binding {
        self.map.borrow().clone()
    }

    pub fn duplicate(&self) -> SymbolEnv {
        SymbolEnv { map: Rc::new(RefCell::new(self.clone_map())) }
    }
}
