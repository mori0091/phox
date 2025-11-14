use std::cell::RefCell;
use std::collections::HashMap;

use super::Path;

thread_local! {
    static SYMBOL_TABLE: RefCell<HashMap<Symbol, Path>> = RefCell::new(HashMap::new());
    static NEXT_GLOBAL_ID: RefCell<usize> = RefCell::new(0);
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GlobalId(usize);

impl GlobalId {
    pub fn new() -> Self {
        NEXT_GLOBAL_ID.with(|next| {
            let id = GlobalId(*next.borrow());
            *next.borrow_mut() += 1;
            id
        })
    }
}

impl std::fmt::Display for GlobalId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Symbol {
    Unresolved(Path),           // `foo`, `foo::Foo`
    Extern(GlobalId),           // `::prelude::map` (extern only)
    Local(String),              // `map`, `foo`, `bar`
}

impl Symbol {
    pub fn register(&self, path: &Path) {
        SYMBOL_TABLE.with(|tbl| {
            tbl.borrow_mut().insert(self.clone(), path.clone());
        });
    }

    pub fn lookup(&self) -> Option<Path> {
        SYMBOL_TABLE.with(|tbl| {
            tbl.borrow().get(self).cloned()
        })
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(path) = self.lookup() {
            write!(f, "{}", path)
        } else {
            match self {
                Symbol::Unresolved(path) => {
                    write!(f, "<unresolved {}>", path)
                }
                Symbol::Local(name) => {
                    write!(f, "{}", name)
                }
                Symbol::Extern(id) => {
                    write!(f, "<extern {}>", id)
                }
            }
        }
    }
}

impl Symbol {
    pub fn pretty(&self) -> String {
        match self {
            Symbol::Local(name) => {
                if name == "()" || name.starts_with('_') || name.starts_with(|c:char| c.is_ascii_alphabetic()) {
                    format!("{}", name)
                }
                else {
                    format!("({})", name)
                }
            }
            other => other.to_string()
        }
    }
}
