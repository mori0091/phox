use std::fmt;
use crate::typesys::{Type, TypeVarId};
use crate::typesys::{TypeError, TypeContext, TypeEnv, instantiate};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Constraint {
    pub name: String,           // trait name (ex. Eq, Ord)
    pub params: Vec<Type>,      // type parameters
}

impl Constraint {
    pub fn from_trait_member (
        ctx: &mut TypeContext,  // global TypeContext
        type_env: &TypeEnv,     // borrow InferCtx.type_env
        member_name: &str,      // name of a member of trait
        member_ty: &Type,       // type of that member
    ) -> Result<Vec<Constraint>, TypeError> {
        // 1. trait メンバの型スキームを取得
        let scheme = type_env.get(member_name)
            .ok_or_else(|| TypeError::UnknownTraitMember(member_name.to_string()))?;
        // 2. スキームを展開（ctx に型変数を割り当てる）
        let (constraints, trait_ty) = instantiate(ctx, scheme);
        // 3. 式の型と unify（ctx.binding に置換が記録される）
        ctx.unify(&trait_ty, member_ty)?;
        // 4. 全制約に置換を適用
        let resolved_constraints = constraints
            .into_iter()
            .map(|mut c| {
                c.params
                    = c.params
                       .into_iter()
                       .map(|t| ctx.repr(&t))
                       .collect();
                c
            })
            .collect();
        Ok(resolved_constraints)
    }
}

// ===== Type schemes (∀ vars . ty) =====
#[derive(Clone, Debug)]
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

        let renamed_ty = rename(&self.ty, &map);

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
