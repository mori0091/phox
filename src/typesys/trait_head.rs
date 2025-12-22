use std::fmt;
use std::collections::{HashMap, HashSet};
use crate::typesys::*;
use crate::module::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TraitHead {
    pub name: Symbol,           // trait name (ex. Eq, Ord)
    pub params: Vec<Type>,      // type parameters
}

impl TraitHead {
    /// Searches for all `trait`s that have members with the specified name and
    /// type.
    pub fn lookup_traits_by_member(
        ctx: &mut TypeContext,
        member_env: &TraitMemberEnv,
        member_name: &Symbol,
        member_ty: &Type,
    ) -> Result<Vec<TraitHead>, Error> {
        let entries = member_env
            .get(member_name)
            .ok_or_else(|| Error::UnknownTraitMember(member_name.to_string()))?;

        let mut out = Vec::new();
        for scheme_tmpl in entries {
            let scheme = scheme_tmpl.fresh_copy(ctx);
            let (constraints, ty) = scheme.instantiate(ctx);
            if ctx.unify(&ty, member_ty).is_ok() {
                if let Some(ref head) = constraints.primary {
                    let mut head = *head.clone();
                    head.params = head.params.into_iter().map(|t| t.repr(ctx)).collect();
                    out.push(head);
                }
            }
        }
        Ok(out)
    }
}

use super::FreeTypeVars;

impl FreeTypeVars for TraitHead {
    fn free_type_vars(&self, ctx: &mut TypeContext, acc: &mut HashSet<TypeVarId>) {
        for t in self.params.iter() {
            t.free_type_vars(ctx, acc);
        }
    }
}

use super::Repr;

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

impl SchemePretty for TraitHead {
    fn rename_type_var(&self, map: &mut HashMap<TypeVarId, String>) -> Self {
        let ts = self.params.iter().map(|t| t.rename_type_var(map)).collect();
        TraitHead {name: self.name.clone(), params: ts}
    }
}

impl Pretty for TraitHead {
    fn pretty(&self) -> String {
        self.rename_type_var(&mut HashMap::new()).to_string()
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

impl TraitHead {
    pub fn score(&self) -> (usize, i64) {
        let mut ret = (0, 0);
        for e in self.params.iter() {
            let (s, g) = e.score();
            ret.0 += s;
            ret.1 += g;
        }
        ret
    }
}
