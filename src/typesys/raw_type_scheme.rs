use crate::syntax::ast::{RawConstraint, RawType};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RawTypeScheme {
    pub vars: Vec<String>,               // `∀ [e]`
    pub constraints: Vec<RawConstraint>, // `Monad (Result e) =>`
    pub target: RawType,                 // `Result e 'a -> ('a -> Result e 'b) -> Result e 'b`
}

use std::fmt;

impl fmt::Display for RawTypeScheme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.pretty())
    }
}

impl fmt::Display for RawConstraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tys = self.params
                    .iter()
                    .map(|ty| ty.to_string())
                    .collect::<Vec<_>>()
            .join(" ");
        write!(f, "{} {}", self.name, tys)
     }
}

impl fmt::Display for RawType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RawType::VarName(name) => write!(f, "{}", name),
            RawType::ConName(name) => write!(f, "{}", name),
            RawType::Fun(a, b) => {
                // 左側は必要なら括弧
                match **a {
                    RawType::Fun(_, _) => write!(f, "({}) -> {}", a, b),
                    _ => write!(f, "{} -> {}", a, b),
                }
            }
            RawType::App(fun, arg) => {
                match **arg {
                    RawType::Fun(_, _) | RawType::App(_, _) => write!(f, "{} ({})", fun, arg),
                    _ => write!(f, "{} {}", fun, arg),
                }
            }
            RawType::Tuple(ts) => {
                assert!(!ts.is_empty());
                if ts.len() == 1 {
                    write!(f, "({},)", ts[0])
                }
                else {
                    let s: Vec<String> = ts.iter().map(|t| t.to_string()).collect();
                    write!(f, "({})", s.join(", "))
                }
            }
            RawType::Record(fields) => {
                if fields.is_empty() {
                    write!(f, "@{{}}")
                }
                else {
                    let s: Vec<String>
                        = fields.iter()
                                .map(|(k, v)| format!("{}: {}", k, v))
                                .collect();
                    write!(f, "@{{ {} }}", s.join(", "))
                }
            }
        }
    }
}

impl RawTypeScheme {
    pub fn pretty(&self) -> String {
        let mut ty_string = self.target.to_string();
        if !self.constraints.is_empty() {
            let cs = self.constraints
                         .iter()
                         .map(|c| c.to_string())
                         .collect::<Vec<_>>()
                .join(", ");
            ty_string = format!("{} => {}", cs, ty_string);
        }

        if self.vars.is_empty() {
            format!("{}", ty_string)
        } else {
            // let vars: Vec<String> = (0..self.vars.len())
            //     .map(|i| ((b'a' + i as u8) as char).to_string())
            //     .collect();
            // format!("∀ {}. {}", vars.join(" "), ty_string)
            format!("∀ {}. {}", self.vars.join(" "), ty_string)
        }
    }
}

use std::collections::{HashMap, HashSet};

impl RawTypeScheme {
    pub fn apply_subst(&self, subst: &HashMap<String, RawType>) -> Self {
        let constraints = self.constraints.iter().map(|c| c.apply_subst(subst)).collect();
        let target = self.target.apply_subst(subst);
        RawTypeScheme { vars: vec![], constraints, target }
    }

    pub fn generalize(&self) -> Self {
        let mut free = HashSet::new();
        for c in self.constraints.iter() {
            c.free_type_vars(&mut free);
        }
        self.target.free_type_vars(&mut free);
        let vars: Vec<_> = free.into_iter().collect();
        let constraints = self.constraints.clone();
        let target = self.target.clone();
        RawTypeScheme { vars, constraints, target }
    }
}

impl RawConstraint {
    pub fn apply_subst(&self, subst: &HashMap<String, RawType>) -> Self {
        let params = self.params.iter().map(|p| p.apply_subst(subst)).collect();
        RawConstraint { name: self.name.clone(), params }
    }

    pub fn free_type_vars(&self, free: &mut HashSet<String>) {
        for t in self.params.iter() {
            t.free_type_vars(free);
        }
    }
}

impl RawType {
    pub fn apply_subst(&self, subst: &HashMap<String, RawType>) -> Self {
        match self {
            RawType::VarName(a) => {
                // subst.get(a).unwrap_or(self).clone()
                match subst.get(a) {
                    Some(v) => v.clone(),
                    None => self.clone(),
                }
            }
            RawType::App(f, x) => {
                RawType::App(Box::new(f.apply_subst(subst)),
                             Box::new(x.apply_subst(subst)))
            }
            RawType::Fun(a, b) => {
                RawType::Fun(Box::new(a.apply_subst(subst)),
                             Box::new(b.apply_subst(subst)))
            }
            RawType::ConName(_) => {
                self.clone()
            }
            RawType::Tuple(es) => {
                RawType::Tuple(
                    es.iter()
                      .map(|e| e.apply_subst(subst))
                      .collect()
                )
            }
            RawType::Record(fields) => {
                RawType::Record(
                    fields.iter()
                          .map(|(name, f)| (name.clone(), f.apply_subst(subst)))
                          .collect()
                )
            }
        }
    }

    pub fn free_type_vars(&self, free: &mut HashSet<String>) {
        match self {
            RawType::VarName(a) => {
                free.insert(a.clone());
            }
            RawType::App(f, x) => {
                f.free_type_vars(free);
                x.free_type_vars(free);
            }
            RawType::Fun(a, b) => {
                a.free_type_vars(free);
                b.free_type_vars(free);
            }
            RawType::ConName(_) => {
                return;
            }
            RawType::Tuple(es) => {
                for e in es.iter() {
                    e.free_type_vars(free);
                }
            }
            RawType::Record(fields) => {
                for (_, ty) in fields.iter() {
                    ty.free_type_vars(free);
                }
            }
        }
    }
}
