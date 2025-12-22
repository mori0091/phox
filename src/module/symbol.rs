use super::{Path, PathComponent};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Symbol {
    Unresolved(Path),   // `foo`, `foo::Foo`
    Local(String),      // `map`, `foo`, `bar`
    Unique(Path),       // `::foo::bar`
}

impl Symbol {
    pub fn unresolved<S: Into<String>>(name: S) -> Self {
        Symbol::Unresolved(Path::relative(vec![name.into()]))
    }
    pub fn local<S: Into<String>>(name: S) -> Self {
        Symbol::Local(name.into())
    }
    pub fn trait_member<S: Into<String>>(name: S) -> Self {
        Symbol::local(name)
    }
    // === for premitive type ===
    pub fn unit() -> Symbol {
        Symbol::Unique(Path::absolute(vec!["core", "()"]))
    }
    pub fn bool_() -> Symbol {
        Symbol::Unique(Path::absolute(vec!["core", "Bool"]))
    }
    pub fn int() -> Symbol {
        Symbol::Unique(Path::absolute(vec!["core", "Int"]))
    }
}

impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::Unresolved(path) => {
                write!(f, "Unresolved({})", path)
            }
            Symbol::Local(name) => {
                write!(f, "Local({})", name)
            }
            Symbol::Unique(path) => {
                write!(f, "Unique({})", path)
            }
        }
    }
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
            Symbol::Unique(path) => {
                // write!(f, "{}", path)
                write!(f, "{}", path.last().unwrap().pretty())
            }
        }
    }
}

impl Symbol {
    pub fn pretty(&self) -> String {
        match self {
            Symbol::Local(name) => {
                PathComponent::Name(name.to_string()).pretty()
            }
            Symbol::Unique(path) => {
                path.last().unwrap().pretty()
            }
            other => other.to_string()
        }
    }
}
