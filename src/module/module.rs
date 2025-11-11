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
    pub name: String,
    pub icx: InferCtx,
    pub env: Env,
    submods: HashMap<String, RefModule>,
    _exports: HashSet<String>,     // for `pub ...`
    using: HashMap<String, Path>, // for `use ... [as ...]`
    parent: Option<WeakRefModule>,
}

pub trait ModuleExt {
    fn add_submod(&self, name: &str) -> RefModule;
}

impl ModuleExt for RefModule {
    fn add_submod(&self, name: &str) -> RefModule {
        let child = Rc::new(RefCell::new(Module {
            name: name.to_string(),
            icx: InferCtx::new(),
            env: Env::new(),
            submods: HashMap::new(),
            _exports: HashSet::new(),
            using: HashMap::new(),
            parent: Some(Rc::downgrade(self)),
        }));
        self.borrow_mut().submods.insert(name.to_string(), child.clone());
        child
    }
}

impl Module {
    pub fn new_root(name: &str) -> RefModule {
        Rc::new(RefCell::new(Module {
            name: name.to_string(),
            icx: InferCtx::initial(),
            env: initial_env(),
            submods: HashMap::new(),
            _exports: HashSet::new(),
            using: HashMap::new(),
            parent: None,
        }))
    }

    pub fn path(&self) -> Path {
        if let Some(parent) = self.parent() {
            parent.borrow().path().concat(&[PathComponent::Name(self.name.clone())])
        }
        else {
            Path::absolute(vec![self.name.clone()])
        }
    }
}

impl Module {
    pub fn add_alias(&mut self, name: &str, path: &Path) -> Result<(), String> {
        if let Some(other) = self.using.get(name) {
            Err(format!("name `{}` is already used as `{}`", name, other))
        }
        else {
            self.using.insert(name.to_string(), path.clone());
            Ok(())
        }
    }
}

impl Module {
    pub fn resolve_alias(&self, path: &Path) -> Option<Path> {
        match path {
            Path::Absolute(_) => Some(path.clone()),
            Path::Relative(xs) => {
                if let Some(alias) = self.using.get(&xs[0].to_string()) {
                    Some(alias.concat(&xs[1..]))
                }
                else {
                    None
                }
            }
        }
    }
}

impl Module {
    pub fn put_var(&mut self, name: Symbol, value: Value) {
        self.env.insert(name, value);
    }

    pub fn get_var(&self, name: &Symbol) -> Option<Value> {
        self.env.get(name)
    }

    pub fn put_type_var(&mut self, name: Symbol, scheme: TypeScheme) {
        self.icx.type_env.insert(name, scheme);
    }

    pub fn get_type_var(&self, name: &Symbol) -> Option<TypeScheme> {
        self.icx.type_env.get(name).cloned()
    }

    pub fn parent(&self) -> Option<RefModule> {
        self.parent.as_ref().and_then(|w| w.upgrade())
    }

    pub fn get_submod(&self, name: &str) -> Option<RefModule> {
        self.submods.get(name).cloned()
    }
}
