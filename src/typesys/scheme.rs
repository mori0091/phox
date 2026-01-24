use std::fmt;
use std::collections::{HashMap, HashSet};
use super::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Var {
    Ty(TypeVarId),
    // Row(RowVarId),
    // Nat(NatVarId),
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ty(v) => write!(f, "{}", v)
        }
    }
}

// ===== Schemes (∀ vars . (constraints) => target) =====
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Scheme<T> {
    pub vars: Vec<Var>,               // quantified variables              (ex. `∀ a.`)
    pub constraints: ConstraintSet,   // constraints / trait bounds        (ex. `(Eq a, Ord a) =>`)
    pub target: T,                    // the type, or                      (ex. `a -> a -> Bool`)
                                      // the trait head produced by `impl` (ex. `Eq (List a)`)
}

impl <T> Scheme<T> {
    pub fn new(vars: Vec<Var>, constraints: ConstraintSet, target: T) -> Scheme<T> {
        Scheme { vars, constraints, target }
    }
    pub fn mono(target: T) -> Scheme<T> {
        Scheme::new(vec![], ConstraintSet::new(), target)
    }
    pub fn poly(vars: Vec<Var>, target: T) -> Scheme<T> {
        Scheme::new(vars, ConstraintSet::new(), target)
    }
}

pub fn generalize<T: FreeVars + Repr>(ctx: &mut TypeContext, icx: &InferCtx, target: &T) -> Scheme<T> {
    let mut fty = HashSet::new();
    target.free_vars(ctx, &mut fty);
    let fenv = icx.free_env_vars(ctx);
    let mut vars: Vec<Var> = fty.difference(&fenv).cloned().collect();
    vars.sort();
    Scheme::poly(vars, target.repr(ctx))
}

impl <T: ApplySubst> ApplySubst for Scheme<T> {
    fn apply_subst(&self, subst: &Subst) -> Self {
        let vars: Vec<_> = self
            .vars
            .iter()
            .filter(|v| !subst.contains_key(*v))
            .cloned()
            .collect();
        let constraints = self.constraints.apply_subst(subst);
        let target = self.target.apply_subst(subst);
        Scheme::new(vars, constraints, target)
    }
}

use super::TypeContext;

impl <T: ApplySubst> Scheme<T> {
    pub fn fresh_copy(&self, ctx: &mut TypeContext) -> Self {
        let mut vars = Vec::new();
        let mut subst: Subst = Subst::new();
        for v in self.vars.iter() {
            let new_var = ctx.fresh_var_id();
            vars.push(Var::Ty(new_var.clone()));
            subst.insert(v.clone(), Type::var(new_var));
        }
        let constraints = self.constraints.apply_subst(&subst);
        let target = self.target.apply_subst(&subst);
        Scheme::new(vars, constraints, target)
    }
}

impl <T: ApplySubst> Scheme<T> {
    pub fn instantiate(&self, ctx: &mut TypeContext) -> (ConstraintSet, T) {
        let mut subst: Subst = Subst::new();
        for v in self.vars.iter() {
            subst.insert(v.clone(), Type::var(ctx.fresh_var_id()));
        }
        let constraints = self.constraints.apply_subst(&subst);
        let target = self.target.apply_subst(&subst);
        (constraints, target)
    }
}

impl <T: fmt::Display> fmt::Display for Scheme<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.vars.is_empty() {
            write!(f, "∀ {}. ", TypeVarList(&self.vars))?;
        }
        if self.constraints.is_empty() {
            write!(f, "{}", self.target)
        } else {
            write!(f, "{} {}", self.target, self.constraints)
        }
    }
}

impl <T: RenameForPretty + Pretty + fmt::Display> Pretty for Scheme<T> {
    fn pretty(&self) -> String {
        // 量化変数に a, b, c... を割り当てる
        let mut map = HashMap::new();
        for (i, v) in self.vars.iter().enumerate() {
            let ch = (b'a' + i as u8) as char;
            map.insert(v.clone(), ch.to_string());
        }

        let mut renamed = self.target.rename_var(&mut map).pretty();

        if !self.constraints.is_empty() {
            let requires = self.constraints.rename_var(&mut map).pretty();
            renamed = format!("{} {}", renamed, requires);
        }

        let mut vars: Vec<String> = map.into_values().collect();
        vars.sort();
        if vars.is_empty() {
            format!("{}", renamed)
        } else {
            format!("∀ {}. {}", vars.join(" "), renamed)
        }
    }
}

struct TypeVarList<'a>(&'a [Var]);

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
