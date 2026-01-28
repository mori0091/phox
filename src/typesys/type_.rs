use indexmap::IndexSet;
use std::fmt;

use super::*;
use crate::module::*;

// ===== Type =====
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Var(TypeVarId),             // 型変数
    Fun(Box<Type>, Box<Type>),  // 関数型
    Con(Symbol),                // 型構築子
    App(Box<TypeExpr>, Box<TypeExpr>), // 型適用

    Tuple(Vec<Type>),
    Record(Vec<(String, Type)>),
}

impl Type {
    fn local_con<S: Into<String>>(s: S) -> Self {
        Type::Con(Symbol::Local(s.into()))
    }
    pub fn unit() -> Self {
        Type::Con(Symbol::unit())
    }
    pub fn bool_() -> Self {
        Type::Con(Symbol::bool_())
    }
    pub fn int() -> Self {
        Type::Con(Symbol::int())
    }
    pub fn var(id: TypeVarId) -> Self {
        Type::Var(id)
    }
    pub fn unresolved_con<S: Into<String>>(s: S) -> Self {
        Type::Con(Symbol::Unresolved(Path::relative(vec![s.into()])))
    }
    pub fn app(f: TypeExpr, x: TypeExpr) -> Self {
        Type::App(Box::new(f), Box::new(x))
    }
    pub fn fun(a: Type, b: Type) -> Self {
        Type::Fun(Box::new(a), Box::new(b))
    }
}

impl Type {
    pub fn is_type_var(&self) -> bool {
        match self {
            Type::Var(_) => true,
            _ => false,
        }
    }

    pub fn contains_type_var(&self) -> bool {
        match self {
            Type::Var(_) => true,
            Type::Con(_) => false,
            Type::App(f, x) => {
                f.contains_type_var() || x.contains_type_var()
            },
            Type::Fun(a, b) => {
                a.contains_type_var() || b.contains_type_var()
            },
            Type::Record(fields) => {
                fields.iter().any(|(_, t)| t.contains_type_var())
            },
            Type::Tuple(tys) => {
                tys.iter().any(|t| t.contains_type_var())
            },
        }
    }
}

impl ApplySubst for Type {
    fn apply_subst(&self, subst: &Subst) -> Self {
        match self {
            Type::Var(id) => subst.get(&Var::Ty(*id)).cloned().unwrap_or(Type::Var(*id)),
            Type::Con(name) => Type::Con(name.clone()),
            Type::Fun(t1, t2) => {
                Type::fun(t1.apply_subst(subst), t2.apply_subst(subst))
            }
            Type::App(t1, t2) => {
                Type::app(t1.apply_subst(subst), t2.apply_subst(subst))
            }
            Type::Tuple(ts) => {
                let ts2 = ts.iter().map(|t| t.apply_subst(subst)).collect();
                Type::Tuple(ts2)
            }
            Type::Record(fields) => {
                let fields2 = fields.iter().map(|(name, t)| (name.clone(), t.apply_subst(subst))).collect();
                Type::Record(fields2)
            }
        }
    }
}

impl FreeVars for Type {
    fn free_vars(&self, ctx: &mut UnifiedContext, acc: &mut IndexSet<Var>) {
        match self.repr(&mut ctx.ty) {
            Type::Var(v) => {
                acc.insert(Var::Ty(v));
            }
            Type::Fun(ref a, ref b) => {
                a.free_vars(ctx, acc);
                b.free_vars(ctx, acc);
            }
            Type::Con(_) => {}
            Type::App(ref a, ref b) => {
                a.free_vars(ctx, acc);
                b.free_vars(ctx, acc);
            }
            Type::Tuple(ts) => {
                for ty in ts {
                    ty.free_vars(ctx, acc);
                }
            }
            Type::Record(ref fields) => {
                for (_, field_ty) in fields {
                    field_ty.free_vars(ctx, acc);
                }
            }
        }
    }
}

use super::Repr;

impl Repr for Type {
    fn repr(&self, ctx: &mut TypeContext) -> Self {
        match self {
            Type::Var(v) => {
                if let Some(bound) = ctx.get_bound(v) {
                    bound.repr(ctx)
                } else {
                    Type::Var(ctx.find(*v))
                }
            }
            Type::Fun(a, b) => Type::fun(a.repr(ctx), b.repr(ctx)),
            Type::App(f, x) => Type::app(f.repr(ctx), x.repr(ctx)),
            Type::Con(c) => Type::Con(c.clone()),

            Type::Tuple(ts) => {
                Type::Tuple(
                    ts.iter()
                      .map(|t| t.repr(ctx)).collect()
                )
            }

            Type::Record(fields) => {
                Type::Record(
                    fields.iter()
                          .map(|(field, t)| (field.clone(), t.repr(ctx)))
                          .collect()
                )
            }
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Var(v) => write!(f, "{}", v),
            Type::Con(name) => write!(f, "{}", name),
            Type::Fun(a, b) => {
                // 左側は必要なら括弧
                match **a {
                    Type::Fun(_, _) => write!(f, "({}) -> {}", a, b),
                    _ => write!(f, "{} -> {}", a, b),
                }
            }
            Type::App(fun, arg) => {
                match **arg {
                    TypeExpr::Ty(ref ty) => match ty {
                        Type::Fun(_, _) | Type::App(_, _) => write!(f, "{} ({})", fun, arg),
                        _ => write!(f, "{} {}", fun, arg),
                    }
                }
            }
            Type::Tuple(ts) => {
                assert!(!ts.is_empty());
                if ts.len() == 1 {
                    write!(f, "({},)", ts[0])
                }
                else {
                    let s: Vec<String> = ts.iter().map(|t| t.to_string()).collect();
                    write!(f, "({})", s.join(", "))
                }
            }
            Type::Record(fields) => {
                if fields.is_empty() {
                    write!(f, "@{{}}")
                }
                else {
                    let s: Vec<String>
                        = fields.iter()
                                .map(|(k, v)| format!("{}: {}", PathComponent::Name(k.clone()).pretty(), v))
                                .collect();
                    write!(f, "@{{ {} }}", s.join(", "))
                }
            }
        }
    }
}

impl RenameForPretty for Type {
    fn rename_var(&self, map: &mut VarNameMap) -> Self {
        match self {
            Type::Var(v) => {
                let v = Var::Ty(v.clone());
                if let Some(name) = map.get(&v) {
                    Type::local_con(name) // ここでは Var を Con に置き換えてもよい
                } else {
                    let ch = (b'a' + map.len() as u8) as char;
                    let name = ch.to_string();
                    map.insert(v, name.clone());
                    Type::local_con(name)
                }
            }
            Type::Con(name) => Type::Con(name.clone()),
            Type::Fun(t1, t2) => {
                Type::fun(t1.rename_var(map), t2.rename_var(map))
            }
            Type::App(t1, t2) => {
                Type::app(t1.rename_var(map), t2.rename_var(map))
            }
            Type::Tuple(ts) => {
                Type::Tuple(ts.iter().map(|t| t.rename_var(map)).collect())
            }
            Type::Record(fields) => {
                Type::Record(
                    fields.iter().map(|(f, t)| (f.clone(), t.rename_var(map))).collect()
                )
            }
        }
    }
}

impl Pretty for Type {
    fn pretty(&self) -> String {
        self.rename_var(&mut VarNameMap::new()).to_string()
   }
}
