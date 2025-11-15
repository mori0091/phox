use std::fmt;
use std::collections::{HashMap, HashSet};
use super::*;

// ===== Schemes (∀ vars . (constraints) => target) =====
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Scheme<T> {
    pub vars: Vec<TypeVarId>,         // quantified variables              (ex. `∀ a.`)
    pub constraints: Vec<TraitHead>,  // constraints / trait bounds        (ex. `(Eq a, Ord a) =>`)
    pub target: T,                    // the type, or                      (ex. `a -> a -> Bool`)
                                      // the constraint produced by `impl` (ex. `Eq (List a)`)
}

impl <T> Scheme<T> {
    pub fn new(vars: Vec<TypeVarId>, constraints: Vec<TraitHead>, target: T) -> Scheme<T> {
        Scheme { vars, constraints, target }
    }
    pub fn mono(target: T) -> Scheme<T> {
        Scheme::new(vec![], vec![], target)
    }
    pub fn poly(vars: Vec<TypeVarId>, target: T) -> Scheme<T> {
        Scheme::new(vars, vec![], target)
    }
}

pub fn generalize<T: FreeTypeVars + Repr>(ctx: &mut TypeContext, icx: &InferCtx, target: &T) -> Scheme<T> {
    let mut fty = HashSet::new();
    target.free_type_vars(ctx, &mut fty);
    let fenv = icx.free_env_vars(ctx);
    let mut vars: Vec<TypeVarId> = fty.difference(&fenv).cloned().collect();
    vars.sort_by_key(|v| v.0);
    Scheme::poly(vars, target.repr(ctx))
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

use super::TypeContext;

impl <T: ApplySubst> Scheme<T> {
    pub fn fresh_copy(&self, ctx: &mut TypeContext) -> Self {
        let mut vars = Vec::new();
        let mut subst: HashMap<TypeVarId, Type> = HashMap::new();
        for &v in self.vars.iter() {
            let new_var = ctx.fresh_type_var_id();
            vars.push(new_var.clone());
            subst.insert(v, Type::var(new_var));
        }

        let target = self.target.apply_subst(&subst);

        let constraints = self.constraints.iter().map(|c| c.apply_subst(&subst)).collect();

        Scheme::new(vars, constraints, target)
    }
}

impl <T: ApplySubst> Scheme<T> {
    pub fn instantiate(&self, ctx: &mut TypeContext) -> (Vec<TraitHead>, T) {
        let mut subst: HashMap<TypeVarId, Type> = HashMap::new();
        for &v in self.vars.iter() {
            subst.insert(v, Type::var(ctx.fresh_type_var_id()));
        }

        let target = self.target.apply_subst(&subst);

        let constraints = self.constraints.iter().map(|c| c.apply_subst(&subst)).collect();

        (constraints, target)
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

impl <T: SchemePretty + fmt::Display> Scheme<T> {
    pub fn pretty(&self) -> String {
        use std::collections::HashMap;

        // 量化変数に a, b, c... を割り当てる
        let mut map = HashMap::new();
        for (i, v) in self.vars.iter().enumerate() {
            let ch = (b'a' + i as u8) as char;
            map.insert(*v, ch.to_string());
        }

        let mut renamed = self.target.rename_type_var(&mut map).to_string();

        if !self.constraints.is_empty() {
            let cs = self.constraints
                         .iter()
                         .map(|c| c.rename_type_var(&mut map).to_string())
                         .collect::<Vec<_>>();
            if cs.len() == 1 {
                renamed = format!("{} => {}", cs[0], renamed);
            }
            else {
                renamed = format!("({}) => {}", cs.join(", "), renamed);
            }
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
