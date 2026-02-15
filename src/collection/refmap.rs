use std::cell::RefCell;
use std::rc::Rc;
use std::hash::Hash;
use indexmap::IndexMap;

#[derive(Clone, Debug)]
pub struct RefMap<K, V> {
    inner: Rc<RefCell<IndexMap<K, V>>>,
}

impl <K: Clone + Eq + Hash, V: Clone> RefMap<K, V> {
    pub fn new() -> Self {
        Self { inner: Rc::new(RefCell::new(IndexMap::new())) }
    }
    pub fn insert(&self, k: K, v: V) {
        self.inner.borrow_mut().insert(k, v);
    }
    pub fn extend(&self, other: &IndexMap<K, V>) {
        self.inner.borrow_mut().extend(other.clone());
    }
    pub fn get(&self, k: &K) -> Option<V> {
        self.inner.borrow().get(k).cloned()
    }
    pub fn contains_key(&self, k: &K) -> bool {
        self.inner.borrow().contains_key(k)
    }
    pub fn clone_inner(&self) -> IndexMap<K, V> {
        self.inner.borrow().clone()
    }
    pub fn duplicate(&self) -> Self {
        Self { inner: Rc::new(RefCell::new(self.clone_inner())) }
    }
}
