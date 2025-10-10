use std::fmt;
use super::Lit;

#[derive(Debug, Clone)]
pub enum Pat {
    Wildcard,                   // `_`
    Lit(Lit),                   // `()`, `true`, `1`, etc.
    Var(String),                // `x`
    Con(String, Vec<Pat>),      // `Cons x xs`
    Tuple(Vec<Pat>),            // `(p,)`, `(p1, p2)`
    Struct(String, Vec<(String, Pat)>),
}

impl Pat {
    pub fn var<S: Into<String>>(s: S) -> Self {
        Pat::Var(s.into())
    }
    pub fn con<S: Into<String>>(s: S, args: Vec<Pat>) -> Self {
        Pat::Con(s.into(), args)
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
                    write!(f, "{}", name)?;
                    let s: Vec<String> = args.iter().map(|arg| arg.to_string()).collect();
                    write!(f, " {}", s.join(" "))
                }
            }
            Pat::Tuple(es) => {
                write!(f, "({},", es[0])?;
                if es.len() > 1 {
                    let s: Vec<String> = es[1..].iter().map(|t| t.to_string()).collect();
                    write!(f, " {}", s.join(", "))?;
                }
                write!(f, ")")
            }
            Pat::Struct(_, _) => todo!(),
        }
    }
}
