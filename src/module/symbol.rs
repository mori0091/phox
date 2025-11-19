use super::Path;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Symbol {
    Unresolved(Path),   // `foo`, `foo::Foo`
    Local(String),      // `map`, `foo`, `bar`
}

impl Symbol {
    pub fn unresolved<S: Into<String>>(name: S) -> Self {
        Symbol::Unresolved(Path::relative(vec![name.into()]))
    }
    pub fn local<S: Into<String>>(name: S) -> Self {
        Symbol::Local(name.into())
    }
    // === for premitive type ===
    pub fn unit() -> Symbol {
        Symbol::local("::prelude::()")
    }
    pub fn bool_() -> Symbol {
        Symbol::local("::prelude::Bool")
    }
    pub fn int() -> Symbol {
        Symbol::local("::prelude::Int")
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::Unresolved(path) => {
                write!(f, "<unresolved {}>", path)
            }
            Symbol::Local(name) => {
                let name = name.split("::").last().unwrap().to_string();
                write!(f, "{}", name)
            }
        }
    }
}

impl Symbol {
    pub fn pretty(&self) -> String {
        match self {
            Symbol::Local(name) => {
                let name = name.split("::").last().unwrap().to_string();
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
