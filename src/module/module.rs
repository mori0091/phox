use std::collections::{HashMap, HashSet};
use std::rc::{Rc, Weak};
use std::cell::RefCell;

use crate::typesys::*;
use crate::interpreter::*;
use super::*;

// use crate::prelude::*;

pub type RefModule = Rc<RefCell<Module>>;
pub type WeakRefModule = Weak<RefCell<Module>>;

pub struct Module {
    pub icx: InferCtx,
    pub env: Env,
    submods: HashMap<String, RefModule>,
    _exports: HashSet<String>,     // for `pub ...`
    _using: HashMap<String, Path>, // for `use ... [as ...]`
    parent: Option<WeakRefModule>,
}

pub trait ModuleExt {
    fn add_submod(&self, name: String) -> RefModule;
}

impl ModuleExt for RefModule {
    fn add_submod(&self, name: String) -> RefModule {
        let child = Rc::new(RefCell::new(Module {
            icx: InferCtx::new(),
            env: Env::new(),
            submods: HashMap::new(),
            _exports: HashSet::new(),
            _using: HashMap::new(),
            parent: Some(Rc::downgrade(self)),
        }));
        self.borrow_mut().submods.insert(name, child.clone());
        child
    }
}

impl Module {
    pub fn new_root(ctx: &mut TypeContext) -> RefModule {
        Rc::new(RefCell::new(Module {
            icx: InferCtx::initial(ctx),
            env: initial_env(),
            submods: HashMap::new(),
            _exports: HashSet::new(),
            _using: HashMap::new(),
            parent: None,
        }))
    }

    pub fn put_var(&mut self, name: String, value: Value) {
        self.env.insert(name, value);
    }

    pub fn get_var(&self, name: &str) -> Option<Value> {
        self.env.get(name)
    }

    pub fn put_type_var(&mut self, name: String, scheme: TypeScheme) {
        self.icx.type_env.insert(name, scheme);
    }

    pub fn get_type_var(&self, name: &str) -> Option<TypeScheme> {
        self.icx.type_env.get(name).cloned()
    }

    pub fn parent(&self) -> Option<RefModule> {
        self.parent.as_ref().and_then(|w| w.upgrade())
    }

    pub fn get_submod(&self, name: &str) -> Option<RefModule> {
        self.submods.get(name).cloned()
    }
}
