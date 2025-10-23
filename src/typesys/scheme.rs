use std::fmt;
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
                    c.params = c.params.into_iter().map(|t| ctx.repr(&t)).collect();
                    c
                });
                out.extend(resolved);
            }
        }
        Ok(out)
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

// ===== Type schemes (∀ vars . ty) =====
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Scheme {
    pub vars: Vec<TypeVarId>, // quantified variables
    pub constraints: Vec<Constraint>,
    pub ty: Type,
}

impl Scheme {
    pub fn new(vars: Vec<TypeVarId>, constraints: Vec<Constraint>, ty: Type) -> Scheme {
        Scheme { vars, constraints, ty }
    }
    pub fn mono(ty: Type) -> Scheme {
        Scheme::new(vec![], vec![], ty)
    }
    pub fn poly(vars: Vec<TypeVarId>, ty: Type) -> Scheme {
        Scheme::new(vars, vec![], ty)
    }
}

impl fmt::Display for Scheme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.vars.is_empty() {
            // 量化変数がなければそのまま型のみ
            write!(f, "{}", self.ty)
        } else {
            write!(f, "∀ {}. {}", TypeVarList(&self.vars), self.ty)
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

impl Scheme {
    pub fn pretty(&self) -> String {
        use std::collections::HashMap;

        // 量化変数に a, b, c... を割り当てる
        let mut map = HashMap::new();
        for (i, v) in self.vars.iter().enumerate() {
            let ch = (b'a' + i as u8) as char;
            map.insert(*v, ch.to_string());
        }

        fn rename(ty: &Type, map: &HashMap<TypeVarId, String>) -> Type {
            match ty {
                Type::Var(v) => {
                    if let Some(name) = map.get(v) {
                        Type::Con(name.clone()) // ここでは Var を Con に置き換えてもよい
                    } else {
                        Type::Var(*v) // 自由変数はそのまま
                    }
                }
                Type::Con(name) => Type::Con(name.clone()),
                Type::Fun(t1, t2) => {
                    Type::fun(rename(t1, map), rename(t2, map))
                }
                Type::App(t1, t2) => {
                    Type::app(rename(t1, map), rename(t2, map))
                }
                Type::Tuple(ts) => {
                    Type::Tuple(ts.iter().map(|t| rename(t, map)).collect())
                }
                Type::Record(fields) => {
                    Type::Record(
                        fields.iter().map(|(f, t)| (f.clone(), rename(t, map))).collect()
                    )
                }
            }
        }

        let mut renamed_ty = rename(&self.ty, &map).to_string();

        if !self.constraints.is_empty() {
            let cs = self.constraints
                          .iter()
                          .map(|c| c.to_string())
                          .collect::<Vec<_>>()
                .join(", ");
            renamed_ty = format!("({}) => {}", cs, renamed_ty);
        }

        if self.vars.is_empty() {
            format!("{}", renamed_ty)
        } else {
            let vars: Vec<String> = (0..self.vars.len())
                .map(|i| ((b'a' + i as u8) as char).to_string())
                .collect();
            format!("∀ {}. {}", vars.join(" "), renamed_ty)
        }
    }
}
