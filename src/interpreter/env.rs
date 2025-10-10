use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use super::Value;

/// 評価時の環境
#[derive(Clone)]
pub struct Env {
    map: Rc<RefCell<HashMap<String, Value>>>,
}

impl Env {
    pub fn new() -> Self {
        Env { map: Rc::new(RefCell::new(HashMap::new())) }
    }

    pub fn insert(&self, k: String, v: Value) {
        self.map.borrow_mut().insert(k, v);
    }

    pub fn get(&self, k: &str) -> Option<Value> {
        self.map.borrow().get(k).cloned()
    }

    pub fn extend(&self, other: &Env) {
        self.map.borrow_mut().extend(other.map.borrow().clone());
    }

    pub fn clone_map(&self) -> HashMap<String, Value> {
        self.map.borrow().clone()
    }

    pub fn duplicate(&self) -> Env {
        Env { map: Rc::new(RefCell::new(self.clone_map())) }
    }
}
