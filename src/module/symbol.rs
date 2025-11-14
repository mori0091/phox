use std::cell::RefCell;

use super::Path;

thread_local! {
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

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

impl Symbol {
    pub fn unresolved(name: &str) -> Self {
        Symbol::Unresolved(Path::relative(vec![name.to_string()]))
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
