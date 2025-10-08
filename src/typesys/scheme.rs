use std::fmt;
use crate::typesys::{Type, TypeVarId};

// ===== Type schemes (∀ vars . ty) =====
#[derive(Clone, Debug)]
pub struct Scheme {
    pub vars: Vec<TypeVarId>, // quantified variables
    pub ty: Type,
}

impl Scheme {
    pub fn mono(t: Type) -> Scheme {
        Scheme { vars: vec![], ty: t, }
    }
    pub fn poly(vs: Vec<TypeVarId>, t: Type) -> Scheme {
        Scheme { vars: vs, ty: t, }
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
                Type::Struct(_, _) => todo!(),
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
