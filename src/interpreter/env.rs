use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use super::Value;
use crate::module::*;

pub type Binding = HashMap<Symbol, Value>;

/// 評価時の環境
#[derive(Clone)]
pub struct ValueEnv {
    map: Rc<RefCell<Binding>>,
}

impl ValueEnv {
    pub fn new() -> Self {
        ValueEnv { map: Rc::new(RefCell::new(HashMap::new())) }
    }

    pub fn insert(&self, k: Symbol, v: Value) {
        self.map.borrow_mut().insert(k, v);
    }

    pub fn get(&self, k: &Symbol) -> Option<Value> {
        self.map.borrow().get(k).cloned()
    }

    pub fn extend(&self, other: &Binding) {
        self.map.borrow_mut().extend(other.clone());
    }

    pub fn clone_map(&self) -> Binding {
        self.map.borrow().clone()
    }

    pub fn duplicate(&self) -> ValueEnv {
        ValueEnv { map: Rc::new(RefCell::new(self.clone_map())) }
    }
}

pub fn make_constructor(name: &Symbol, arity: usize) -> Value {
    // 部分適用を保持する内部関数
    fn curry(name: Symbol, arity: usize, args: Vec<Value>) -> Value {
        if args.len() == arity {
            Value::Con(name, args)
        } else {
            Value::Builtin(Rc::new(move |arg: Value| {
                let mut new_args = args.clone();
                new_args.push(arg);
                curry(name.clone(), arity, new_args)
            }))
        }
    }
    curry(name.clone(), arity, vec![])
}
