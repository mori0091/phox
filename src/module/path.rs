use super::*;

#[derive(Clone, Debug)]
pub enum Path {
    Absolute(Vec<String>), // ::foo::bar
    Relative(Vec<String>), // foo::bar, super::baz
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
