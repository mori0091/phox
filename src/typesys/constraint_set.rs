use std::fmt;
use std::collections::BTreeSet;
use std::collections::HashMap;
use super::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ConstraintSet {
    pub primary: Option<Box<TraitHead>>,
    pub requires: BTreeSet<Constraint>,
}

impl ConstraintSet {
    pub fn new() -> ConstraintSet {
        ConstraintSet {
            primary: None,
            requires: BTreeSet::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.primary.is_none() && self.requires.is_empty()
    }

    pub fn into_vec(&self) -> Vec<Constraint> {
        let mut cs = Vec::new();
        if let Some(ref head) = &self.primary {
            cs.push(Constraint::trait_bound(head));
        }
        let mut xs = self.requires.iter().cloned().collect::<Vec<_>>();
        xs.sort();
        cs.extend(xs);
        cs
    }

    pub fn into_vec_for_trait_record(&self) -> Vec<Constraint> {
        let mut xs = self.requires.iter().cloned().collect::<Vec<_>>();
        xs.sort();
        xs
    }
}

impl ApplySubst for ConstraintSet {
    fn apply_subst(&self, subst: &Subst) -> Self {
        let primary = match self.primary {
            None => None,
            Some(ref head) => Some(Box::new(head.apply_subst(subst))),
        };
        let requires = self.requires.iter().map(|c| c.apply_subst(subst)).collect::<BTreeSet<_>>();
        ConstraintSet {
            primary,
            requires,
        }
    }
}

impl RenameForPretty for ConstraintSet {
    fn rename_var(&self, map: &mut HashMap<Var, String>) -> Self {
        let primary = match self.primary {
            None => None,
            Some(ref head) => Some(Box::new(head.rename_var(map))),
        };
        let requires = self.requires.iter().map(|c| c.rename_var(map)).collect::<BTreeSet<_>>();
        ConstraintSet {
            primary,
            requires,
        }
    }
}

impl Pretty for ConstraintSet {
    fn pretty(&self) -> String {
        let cs = self.into_vec();
        if cs.is_empty() {
            return "".to_string()
        }
        format!("requires {}.", cs.iter().map(|c| c.pretty()).collect::<Vec<_>>().join(", "))
    }
}

impl fmt::Display for ConstraintSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let cs = self.into_vec();
        if cs.is_empty() {
            return write!(f, "")
        }
        write!(f, "requires {}.", cs.iter().map(|c| c.to_string()).collect::<Vec<_>>().join(", "))
    }
}
