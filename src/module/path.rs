use super::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Path {
    Absolute(Vec<String>), // ::foo::bar
    Relative(Vec<String>), // foo::bar, super::baz
}

impl Path {
    pub fn concat(&self, child: &[String]) -> Path {
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

use std::fmt;

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Path::Absolute(xs) => write!(f, "::{}", xs.join("::")),
            Path::Relative(xs) => write!(f, "{}", xs.join("::")),
        }
    }
}

impl Path {
    pub fn pretty(&self) -> String {
        fn normalize(xs: &Vec<String>) -> String {
            xs.iter().map(|x| Symbol::Local(x.to_string()).pretty()).collect::<Vec<_>>().join("::")
        }
        match self {
            Path::Absolute(xs) => format!("::{}", normalize(xs)),
            Path::Relative(xs) => format!("{}", normalize(xs)),
        }
    }
}

pub fn resolve_path(path: &Path, current: RefModule, roots: &RootModules) -> Option<RefModule> {
    match path {
        Path::Absolute(segments) => {
            let mut m = roots.get(&segments[0])?;
            for seg in &segments[1..] {
                let sub = m.borrow().get_submod(seg)?;
                m = sub;
            }
            Some(m)
        }
        Path::Relative(segments) => {
            let mut m = current.clone();
            for seg in segments {
                if seg == ".." {
                    let p = m.borrow().parent()?;
                    m = p;
                } else {
                    let sub = m.borrow().get_submod(seg)?;
                    m = sub;
                }
            }
            Some(m)
        }
    }
}
