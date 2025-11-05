use std::collections::BTreeSet;

#[derive(Debug, Clone)]
pub enum PathGlob {
    Absolute(PathGlobNode),     // `::foo::bar::{baz, quax::*}`
    Relative(PathGlobNode),     // `foo::bar::{baz, quax::*}`
}

#[derive(Debug, Clone)]
pub enum PathGlobNode {
    Leaf(PathGlobLeaf),
    Set(BTreeSet<PathGlobNode>), // `{foo, foo::bar}`
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum PathGlobLeaf {
    Path(Vec<String>, Option<String>), // `foo::bar`, `foo::bar as baz`
    Wildcard(Vec<String>),             // `foo::bar::*`, `*`
}

// -------------------------------------------------------------
use std::fmt;

impl fmt::Display for PathGlob {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathGlob::Absolute(p) => write!(f, "::{}", p),
            PathGlob::Relative(p) => write!(f, "{}", p),
        }
    }
}

impl fmt::Display for PathGlobNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathGlobNode::Leaf(p) => write!(f, "{}", p),
            PathGlobNode::Set(ps) => {
                let s = ps.iter().map(|x| x.to_string()).collect::<Vec<_>>();
                write!(f, "{{{}}}", s.join(", "))
            }
        }
    }
}

impl fmt::Display for PathGlobLeaf {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathGlobLeaf::Path(ps, opt) => {
                let p = ps.join("::");
                match opt {
                    None => write!(f, "{}", p),
                    Some(alias) => write!(f, "{} as {}", p, alias),
                }
            }
            PathGlobLeaf::Wildcard(ps) => {
                if ps.is_empty() {
                    write!(f, "*")
                } else {
                    let p = ps.join("::");
                    write!(f, "{}::*", p)
                }
            }
        }
    }
}
