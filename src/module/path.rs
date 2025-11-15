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
    pub fn len(&self) -> usize {
        match self {
            Path::Absolute(ps) | Path::Relative(ps) => ps.len()
        }
    }

    pub fn head(&self) -> Option<PathComponent> {
        match self {
            Path::Absolute(ps) | Path::Relative(ps) => {
                if ps.is_empty() {
                    None
                } else {
                    Some(ps[0].clone())
                }
            }
        }
    }

    pub fn components(&self) -> Vec<PathComponent> {
        match self {
            Path::Absolute(xs) | Path::Relative(xs) => xs.clone()
        }
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

    pub fn concat_path(&self, other: &Path) -> Path {
        match other {
            Path::Absolute(_)  => other.clone(),
            Path::Relative(ys) => self.concat(&ys),
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

impl Path {
    pub fn resolve(&self, current: &RefModule, roots: &RootModules) -> Option<(RefModule, Option<Path>)> {
        match self {
            Path::Absolute(segments) => {
                match &segments[0] {
                    PathComponent::Wildcard => unreachable!(),
                    PathComponent::Name(name) => {
                        let mut m = roots.get(name)?;
                        for (i, seg) in segments.iter().enumerate().skip(1) {
                            match seg {
                                PathComponent::Name(name) => {
                                    let tmp = m.borrow().get_submod(name);
                                    if let Some(sub) = tmp {
                                        m = sub;
                                    } else {
                                        let rem = Path::Relative(segments[i..].to_vec());
                                        return Some((m.clone(), Some(rem)));
                                    }
                                }
                                PathComponent::Wildcard => {
                                    let rem = Path::Relative(segments[i..].to_vec());
                                    return Some((m.clone(), Some(rem)));
                                }
                            }
                        }
                        Some((m, None))
                    }
                }
            }
            Path::Relative(segments) => {
                let mut m = current.clone();
                for (i, seg) in segments.iter().enumerate() {
                    match seg {
                        PathComponent::Name(name) => {
                            let tmp = m.borrow().get_submod(name);
                            if let Some(sub) = tmp {
                                m = sub;
                            } else {
                                let rem = Path::Relative(segments[i..].to_vec());
                                return Some((m.clone(), Some(rem)));
                            }
                        }
                        PathComponent::Wildcard => {
                            let rem = Path::Relative(segments[i..].to_vec());
                            return Some((m.clone(), Some(rem)));
                        }
                    }
                }
                Some((m, None))
            }
        }
    }
}
