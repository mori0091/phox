use std::fmt;
use std::collections::{HashMap, HashSet};
use crate::typesys::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TraitHead {
    pub name: String,           // trait name (ex. Eq, Ord)
    pub params: Vec<Type>,      // type parameters
}

impl TraitHead {
    pub fn from_trait_member(
        ctx: &mut TypeContext,
        member_env: &TraitMemberEnv,
        member_name: &str,
        member_ty: &Type,
    ) -> Result<Vec<TraitHead>, TypeError> {
        let entries = member_env
            .get(member_name)
            .ok_or_else(|| TypeError::UnknownTraitMember(member_name.to_string()))?;

        let mut out = Vec::new();
        for raw_scheme in entries {
            let scheme = TypeScheme::from(raw_scheme, ctx);
            let (constraints, trait_ty) = scheme.instantiate(ctx);
            if ctx.unify(&trait_ty, member_ty).is_ok() {
                let resolved = constraints.into_iter().map(|mut c| {
                    c.params = c.params.into_iter().map(|t| t.repr(ctx)).collect();
                    c
                });
                out.extend(resolved);
            }
        }
        Ok(out)
    }
}

use super::{FreeTypeVars, TypeScheme};

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
    pub fn unify(&self, ctx: &mut TypeContext, other: &TraitHead) -> Result<(), TypeError> {
        if self.name != other.name {
            return Err(TypeError::UnificationFail {
                expected: self.clone(),
                actual: other.clone(),
            });
        }
        if self.params.len() != other.params.len() {
            return Err(TypeError::ArityMismatch {
                trait_name: self.name.clone(),
                member: "".to_string(),
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
                    .map(|ty| ty.to_string())
                    .collect::<Vec<_>>()
            .join(" ");
        write!(f, "{} {}", self.name, tys)
     }
}

impl ApplySubst for TraitHead {
    fn apply_subst(&self, subst: &HashMap<TypeVarId, Type>) -> Self {
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
