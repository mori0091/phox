use std::fmt;
use std::collections::HashMap;
use crate::typesys::ApplySubst;
use crate::typesys::{TypeVarId, Type, Constraint};

// ===== Schemes (∀ vars . (constraints) => target) =====
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Scheme<T> {
    pub vars: Vec<TypeVarId>,         // quantified variables              (ex. `∀ a.`)
    pub constraints: Vec<Constraint>, // constraints / trait bounds        (ex. `(Eq a, Ord a) =>`)
    pub target: T,                    // the type, or                      (ex. `a -> a -> Bool`)
                                      // the constraint produced by `impl` (ex. `Eq (List a)`)
}

impl <T> Scheme<T> {
    pub fn new(vars: Vec<TypeVarId>, constraints: Vec<Constraint>, target: T) -> Scheme<T> {
        Scheme { vars, constraints, target }
    }
    pub fn mono(target: T) -> Scheme<T> {
        Scheme::new(vec![], vec![], target)
    }
    pub fn poly(vars: Vec<TypeVarId>, target: T) -> Scheme<T> {
        Scheme::new(vars, vec![], target)
    }
}

impl <T: ApplySubst> ApplySubst for Scheme<T> {
    fn apply_subst(&self, subst: &HashMap<TypeVarId, Type>) -> Self {
        let vars: Vec<_> = self
            .vars
            .iter()
            .filter(|v| !subst.contains_key(*v))
            .cloned()
            .collect();
        let constraints = self
            .constraints
            .iter()
            .map(|c| c.apply_subst(subst))
            .collect();
        let target = self
            .target.apply_subst(subst);
        Scheme::new(vars, constraints, target)
    }
}

impl <T: fmt::Display> fmt::Display for Scheme<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.vars.is_empty() {
            // 量化変数がなければそのまま型のみ
            write!(f, "{}", self.target)
        } else {
            write!(f, "∀ {}. {}", TypeVarList(&self.vars), self.target)
        }
    }
}

struct TypeVarList<'a>(&'a [TypeVarId]);

impl<'a> fmt::Display for TypeVarList<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = self.0
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
            .join(" ");
        write!(f, "{}", s)
    }
}
