use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct RefStack<T> {
    inner: Rc<RefCell<Vec<T>>>,
}

impl <T: Clone> RefStack<T> {
    pub fn new() -> Self {
        Self { inner: Rc::new(RefCell::new(Vec::new())) }
    }
    pub fn is_empty(&self) -> bool {
        self.inner.borrow().is_empty()
    }
    pub fn push(&self, v: T) {
        self.inner.borrow_mut().push(v);
    }
    pub fn pop(&self) -> Option<T> {
        self.inner.borrow_mut().pop()
    }
    pub fn clone_inner(&self) -> Vec<T> {
        self.inner.borrow().clone()
    }
    pub fn duplicate(&self) -> Self {
        Self { inner: Rc::new(RefCell::new(self.clone_inner())) }
    }
}
