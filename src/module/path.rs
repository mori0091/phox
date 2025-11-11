use super::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PathComponent {
    Name(String),
    Wildcard,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Path {
    Absolute(Vec<PathComponent>), // ::foo::bar
    Relative(Vec<PathComponent>), // foo::bar, super::baz
}

impl Path {
    pub fn absolute(xs: Vec<String>) -> Path {
        Path::Absolute(xs.into_iter().map(PathComponent::Name).collect())
    }

    pub fn relative(xs: Vec<String>) -> Path {
        Path::Relative(xs.into_iter().map(PathComponent::Name).collect())
    }
}

impl Path {
    pub fn concat(&self, child: &[PathComponent]) -> Path {
        match self {
            Path::Absolute(xs) => {
                let mut ys = xs.clone();
                ys.extend(child.iter().cloned());
                Path::Absolute(ys)
            }
            Path::Relative(xs) => {
                let mut ys = xs.clone();
                ys.extend(child.iter().cloned());
                Path::Relative(ys)
            }
        }
    }
}

impl PathComponent {
    pub fn pretty(&self) -> String {
        match self {
            PathComponent::Name(name) => Symbol::Local(name.clone()).pretty(),
            PathComponent::Wildcard => format!("{}", self),
        }
    }
}

impl Path {
    pub fn pretty(&self) -> String {
        fn normalize(xs: &Vec<PathComponent>) -> String {
            xs.iter().map(|x| x.pretty()).collect::<Vec<_>>().join("::")
        }
        match self {
            Path::Absolute(xs) => format!("::{}", normalize(xs)),
            Path::Relative(xs) => format!("{}", normalize(xs)),
        }
    }
}

use std::fmt;

impl fmt::Display for PathComponent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // PathComponent::Name(name) => write!(f, "{}", Symbol::Local(name.clone()).pretty()),
            PathComponent::Name(name) => write!(f, "{}", name),
            PathComponent::Wildcard => write!(f, "<wildcard>"),
        }
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn normalize(xs: &Vec<PathComponent>) -> String {
            xs.iter().map(|x| x.to_string()).collect::<Vec<_>>().join("::")
        }
        match self {
            Path::Absolute(xs) => {
                write!(f, "::{}", normalize(xs))
            }
            Path::Relative(xs) => {
                write!(f, "{}", normalize(xs))
            }
        }
    }
}

pub fn resolve_path(path: &Path, current: RefModule, roots: &RootModules) -> Option<RefModule> {
    match path {
        Path::Absolute(segments) => {
            match &segments[0] {
                PathComponent::Wildcard => todo!(), // unreachable!() ?
                PathComponent::Name(name) => {
                    let mut m = roots.get(&name)?;
                    for seg in &segments[1..] {
                        match seg {
                            PathComponent::Wildcard => todo!(),
                            PathComponent::Name(name) => {
                                let sub = m.borrow().get_submod(name)?;
                                m = sub;
                            }
                        }
                    }
                    Some(m)
                }
            }
        }
        Path::Relative(segments) => {
            let mut m = current.clone();
            for seg in segments {
                match seg {
                    PathComponent::Wildcard => todo!(),
                    PathComponent::Name(name) => {
                        let sub = m.borrow().get_submod(name)?;
                        m = sub;
                        // if seg == ".." {
                        //     let p = m.borrow().parent()?;
                        //     m = p;
                        // } else {
                        //     let sub = m.borrow().get_submod(seg)?;
                        //     m = sub;
                        // }
                    }
                }
            }
            Some(m)
        }
    }
}
