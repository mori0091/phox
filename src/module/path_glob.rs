#[derive(Debug, Clone)]
pub enum PathGlob {
    Absolute(PathGlobNode),     // `::foo::bar::{baz, quax::*}`
    Relative(PathGlobNode),     // `foo::bar::{baz, quax::*}`
}

#[derive(Debug, Clone)]
pub enum PathGlobNode {
    List(Vec<String>, PathGlobLeaf),      // `ab::cd::foo as bar`
    Tree(Vec<String>, Vec<PathGlobNode>), // `ab::cd::{foo, foo::bar}`
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum PathGlobLeaf {
    Elem(String, Option<String>), // `foo`, `foo as bar`
    Wildcard,                     // `*`
}

// -------------------------------------------------------------
use crate::module::{Path, PathComponent};

impl PathGlob {
    pub fn flatten(&self) -> Vec<(String, Path)> {
        match self {
            PathGlob::Absolute(tree) => tree.flatten()
                .into_iter()
                .map(|(a, p)| match p {
                    Path::Relative(xs) => (a, Path::Absolute(xs)),
                    p => (a, p),
                })
                .collect(),
            PathGlob::Relative(tree) => tree.flatten(),
        }
    }
}

impl PathGlobNode {
    pub fn flatten(&self) -> Vec<(String, Path)> {
        match self {
            PathGlobNode::List(ps, leaf) => match leaf {
                PathGlobLeaf::Elem(name, alias) => {
                    let alias = alias.clone().unwrap_or_else(|| name.clone());
                    let mut ps = ps.clone();
                    ps.push(name.clone());
                    vec![(alias, Path::relative(ps))]
                }
                PathGlobLeaf::Wildcard => {
                    let mut ps = ps
                        .iter()
                        .map(|s| PathComponent::Name(s.clone()))
                        .collect::<Vec<_>>();
                    ps.push(PathComponent::Wildcard);
                    vec![("<wildcard>".to_string(), Path::Relative(ps))]
                }
            },
            PathGlobNode::Tree(ps, nodes) => nodes
                .iter()
                .flat_map(|node| {
                    node.flatten().into_iter().map(|(a, p)| {
                        let p = match p {
                            Path::Relative(xs) => {
                                let mut xss = ps
                                    .iter()
                                    .map(|s| PathComponent::Name(s.clone()))
                                    .collect::<Vec<_>>();
                                xss.extend(xs);
                                Path::Relative(xss)
                            }
                            Path::Absolute(_) => p,
                        };
                        (a, p)
                    })
                })
                .collect(),
        }
    }
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
            PathGlobNode::List(ps, leaf) => {
                if ps.is_empty() {
                    write!(f, "{}", leaf)
                }
                else {
                    let s = ps.iter().map(|x| x.to_string()).collect::<Vec<_>>();
                    write!(f, "{}::{}", s.join("::"), leaf)
                }
            }
            PathGlobNode::Tree(ps, tree) => {
                let t = tree.iter().map(|x| x.to_string()).collect::<Vec<_>>();
                if ps.is_empty() {
                    write!(f, "{{{}}}", t.join(", "))
                }
                else {
                    let s = ps.iter().map(|x| x.to_string()).collect::<Vec<_>>();
                    write!(f, "{}::{{{}}}", s.join("::"), t.join(", "))
                }
            }
        }
    }
}

use crate::module::Symbol;

impl fmt::Display for PathGlobLeaf {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathGlobLeaf::Elem(p, opt) => {
                let p = Symbol::Local(p.clone());
                match opt {
                    None => write!(f, "{}", p.pretty()),
                    Some(alias) => {
                        let alias = Symbol::Local(alias.clone());
                        write!(f, "{} as {}", p.pretty(), alias.pretty())
                    }
                }
            }
            PathGlobLeaf::Wildcard => {
                write!(f, "*")
            }
        }
    }
}
