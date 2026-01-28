use std::rc::Rc;
use std::cell::RefCell;
use indexmap::IndexMap;
use super::*;

pub type Binding = IndexMap<Path, Symbol>;

/// resolve 時の環境
#[derive(Clone)]
pub struct SymbolEnv {
    map: Rc<RefCell<Binding>>,
    local: bool,
}

impl SymbolEnv {
    pub fn new() -> Self {
        SymbolEnv { map: Rc::new(RefCell::new(IndexMap::new())), local: false, }
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
        SymbolEnv { map: Rc::new(RefCell::new(self.clone_map())), local: true, }
    }

    pub fn is_local(&self) -> bool {
        self.local
    }
}
