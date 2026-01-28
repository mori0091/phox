use std::fmt;
use indexmap::IndexSet;

use crate::typesys::*;
use crate::module::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TraitHead {
    pub name: Symbol,           // trait name (ex. Eq, Ord)
    pub params: Vec<Type>,      // type parameters
}

impl FreeVars for TraitHead {
    fn free_vars(&self, ctx: &mut UnifiedContext, acc: &mut IndexSet<Var>) {
        for t in self.params.iter() {
            t.free_vars(ctx, acc);
        }
    }
}

impl Repr for TraitHead {
    fn repr(&self, ctx: &mut TypeContext) -> Self {
        let name = self.name.clone();
        let params = self.params.iter().map(|t| t.repr(ctx)).collect();
        TraitHead { name, params }
    }
}

impl TraitHead {
    pub fn unify(&self, ctx: &mut TypeContext, other: &TraitHead) -> Result<(), Error> {
        if self.name != other.name {
            return Err(Error::UnificationFail {
                expected: self.clone(),
                actual: other.clone(),
            });
        }
        if self.params.len() != other.params.len() {
            return Err(Error::TraitArityMismatch {
                trait_name: self.name.clone(),
                expected: self.params.len(),
                actual: other.params.len(),
            });
        }
        for (a, b) in self.params.iter().zip(&other.params) {
            ctx.unify(a, b)?;
        }
        Ok(())
    }
}

impl fmt::Display for TraitHead {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tys = self.params
                    .iter()
                    .map(|ty| {
                        match ty {
                            Type::App(_, _) | Type::Fun(_, _) => {
                                format!("({})", ty)
                            }
                            _ => {
                                format!("{}", ty)
                            }
                        }
                    })
                    .collect::<Vec<_>>()
            .join(" ");
        write!(f, "{} {}", self.name, tys)
     }
}

impl RenameForPretty for TraitHead {
    fn rename_var(&self, map: &mut VarNameMap) -> Self {
        let ts = self.params.iter().map(|t| t.rename_var(map)).collect();
        TraitHead {name: self.name.clone(), params: ts}
    }
}

impl Pretty for TraitHead {
    fn pretty(&self) -> String {
        self.rename_var(&mut VarNameMap::new()).to_string()
    }
}

impl ApplySubst for TraitHead {
    fn apply_subst(&self, subst: &Subst) -> Self {
        TraitHead {
            name: self.name.clone(),
            params: self.params.iter().map(|t| t.apply_subst(subst)).collect(),
        }
    }
}
