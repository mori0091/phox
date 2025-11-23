use std::collections::{HashMap, HashSet};
use std::rc::{Rc, Weak};
use std::cell::RefCell;

use crate::typesys::*;
use super::*;

// use crate::prelude::*;

pub type RefModule = Rc<RefCell<Module>>;
pub type WeakRefModule = Weak<RefCell<Module>>;

pub struct Module {
    pub name: String,
    pub trait_members: HashMap<String, Vec<String>>, // ex. `{"Eq": ["==", "!="]}`
    submods: HashMap<String, RefModule>,
    _exports: HashSet<String>,     // for `pub ...`
    pub using: HashMap<String, Path>, // for `use ... [as ...]`
    parent: Option<WeakRefModule>,
}

pub trait ModuleExt {
    fn add_submod(&self, name: &str) -> RefModule;
    fn get_submod(&self, name: &str) -> Option<RefModule>;
}

impl ModuleExt for RefModule {
    fn add_submod(&self, name: &str) -> RefModule {
        let child = Rc::new(RefCell::new(Module {
            name: name.to_string(),
            trait_members: HashMap::new(),
            submods: HashMap::new(),
            _exports: HashSet::new(),
            using: HashMap::new(),
            parent: Some(Rc::downgrade(self)),
        }));
        self.borrow_mut().submods.insert(name.to_string(), child.clone());
        child
    }
    fn get_submod(&self, name: &str) -> Option<RefModule> {
        self.borrow().submods.get(name).cloned()
    }
}

impl Module {
    pub fn new_root(name: &str) -> RefModule {
        Rc::new(RefCell::new(Module {
            name: name.to_string(),
            trait_members: HashMap::new(),
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
    pub fn add_alias(&mut self, name: &str, path: &Path) -> Result<(), TypeError> {
        if let Some(other) = self.using.get(name) {
            if other == path {
                Ok(())
            }
            else {
                Err(TypeError::ConflictAlias { name: name.to_string(), other: other.clone() })
            }
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
    pub fn parent(&self) -> Option<RefModule> {
        self.parent.as_ref().and_then(|w| w.upgrade())
    }
}
