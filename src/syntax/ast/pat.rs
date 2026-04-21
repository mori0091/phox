use std::fmt;
use super::Lit;
use crate::module::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Pat {
    Wildcard,                   // `_`
    Lit(Lit),                   // `()`, `true`, `1`, etc.
    Var(Symbol),                // `x`
    Con(Symbol, Vec<Pat>),      // `Cons x xs`
    Tuple(Vec<Pat>),            // `(p,)`, `(p1, p2)`
    Record(Vec<(String, Pat)>),
    Array(Vec<Pat>, Option<PatRest>), // `@[]`, `@[p, ps..]`
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PatRest {
    Named(Symbol),              // `ps..`
    Any,                        // `..`
}

impl Pat {
    pub fn local_var<S: Into<String>>(s: S) -> Self {
        Pat::Var(Symbol::local(s))
    }
    pub fn unresolved_var<S: Into<String>>(s: S) -> Self {
        Pat::Var(Symbol::unresolved(s))
    }
    pub fn unresolved_con<S: Into<String>>(s: S, args: Vec<Pat>) -> Self {
        Pat::Con(Symbol::unresolved(s), args)
    }
    pub fn unresolved_qcon(path: Path, args: Vec<Pat>) -> Self {
        Pat::Con(Symbol::Unresolved(path), args)
    }
}

impl fmt::Display for Pat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pat::Lit(a) => write!(f, "{}", a),
            Pat::Var(x) => write!(f, "{}", x),
            Pat::Wildcard => write!(f, "_"),
            Pat::Con(name, args) => {
                if args.is_empty() {
                    write!(f, "{}", name)
                }
                else {
                    let s: Vec<String> = args.iter().map(|arg| arg.to_string()).collect();
                    write!(f, "{} {}", name, s.join(" "))
                }
            }
            Pat::Tuple(es) => {
                assert!(!es.is_empty());
                if es.len() == 1 {
                    write!(f, "({},)", es[0])
                }
                else {
                    let s: Vec<String> = es.iter().map(|t| t.to_string()).collect();
                    write!(f, "({})", s.join(", "))
                }
            }
            Pat::Record(fields) => {
                if fields.is_empty() {
                    write!(f, "@{{}}")
                }
                else {
                    let s: Vec<String>
                        = fields.iter()
                                .map(|(k, v)| format!("{} = {}", k, v))
                                .collect();
                    write!(f, "@{{ {} }}", s.join(", "))
                }
            }
            Pat::Array(ps, rest) => {
                let mut xs: Vec<_>
                    = ps.iter()
                        .map(|p| format!("{}", p))
                        .collect();
                if let Some(rest) = rest {
                    match rest {
                        PatRest::Named(s) => xs.push(format!("{}..", s)),
                        PatRest::Any => xs.push("..".to_string()),
                    }
                }
                write!(f, "@[{}]", xs.join(", "))
            }
        }
    }
}
