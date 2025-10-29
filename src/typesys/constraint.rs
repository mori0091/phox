use std::fmt;
use std::collections::{HashMap, HashSet};
use crate::typesys::ApplySubst;
use crate::typesys::{Type, TypeVarId};
use crate::typesys::{TypeError, TypeContext, TraitMemberEnv, instantiate};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Constraint {
    pub name: String,           // trait name (ex. Eq, Ord)
    pub params: Vec<Type>,      // type parameters
}

impl Constraint {
    pub fn from_trait_member(
        ctx: &mut TypeContext,
        member_env: &TraitMemberEnv,
        member_name: &str,
        member_ty: &Type,
    ) -> Result<Vec<Constraint>, TypeError> {
        let entries = member_env
            .get(member_name)
            .ok_or_else(|| TypeError::UnknownTraitMember(member_name.to_string()))?;

        let mut out = Vec::new();
        for scheme in entries {
            let (constraints, trait_ty) = instantiate(ctx, scheme);
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

use super::FreeTypeVars;

impl FreeTypeVars for Constraint {
    fn free_type_vars(&self, ctx: &mut TypeContext, acc: &mut HashSet<TypeVarId>) {
        for t in self.params.iter() {
            t.free_type_vars(ctx, acc);
        }
    }
}

use super::Repr;

impl Repr for Constraint {
    fn repr(&self, ctx: &mut TypeContext) -> Self {
        let name = self.name.clone();
        let params = self.params.iter().map(|t| t.repr(ctx)).collect();
        Constraint { name, params }
    }
}

impl Constraint {
    pub fn unify(&self, ctx: &mut TypeContext, other: &Constraint) -> Result<(), TypeError> {
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

impl fmt::Display for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tys = self.params
                    .iter()
                    .map(|ty| ty.to_string())
                    .collect::<Vec<_>>()
            .join(" ");
        write!(f, "{} {}", self.name, tys)
     }
}

impl ApplySubst for Constraint {
    fn apply_subst(&self, subst: &HashMap<TypeVarId, Type>) -> Self {
        Constraint {
            name: self.name.clone(),
            params: self.params.iter().map(|t| t.apply_subst(subst)).collect(),
        }
    }
}

impl Constraint {
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
