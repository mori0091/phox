use std::fmt;
use super::RawTypeScheme;
use super::ApplySubst;

// ===== Types =====
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Var(TypeVarId),             // 型変数
    Fun(Box<Type>, Box<Type>),  // 関数型
    Con(String),                // 型構築子
    App(Box<Type>, Box<Type>),  // 型適用

    Tuple(Vec<Type>),
    Record(Vec<(String, Type)>),

    Overloaded(String, Vec<RawTypeScheme>),
}

impl Type {
    pub fn unit() -> Self {
        Type::con("()")
    }
    pub fn bool_() -> Self {
        Type::con("Bool")
    }
    pub fn int() -> Self {
        Type::con("Int")
    }
    pub fn var(id: TypeVarId) -> Self {
        Type::Var(id)
    }
    pub fn con<S: Into<String>>(s: S) -> Self {
        Type::Con(s.into())
    }
    pub fn app(f: Type, x: Type) -> Self {
        Type::App(Box::new(f), Box::new(x))
    }
    pub fn fun(a: Type, b: Type) -> Self {
        Type::Fun(Box::new(a), Box::new(b))
    }
}

use std::collections::HashMap;

impl ApplySubst for Type {
    fn apply_subst(&self, subst: &HashMap<TypeVarId, Type>) -> Self {
        match self {
            Type::Var(id) => subst.get(id).cloned().unwrap_or(Type::Var(*id)),
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
            Type::Overloaded(_, _) => {
                todo!()
            }
        }
    }
}

use std::collections::HashSet;
use super::TypeContext;
use super::FreeTypeVars;

impl FreeTypeVars for Type {
    fn free_type_vars(&self, ctx: &mut TypeContext, acc: &mut HashSet<TypeVarId>) {
        match self.repr(ctx) {
            Type::Var(v) => {
                acc.insert(v);
            }
            Type::Fun(ref a, ref b) => {
                a.free_type_vars(ctx, acc);
                b.free_type_vars(ctx, acc);
            }
            Type::Con(_) => {}
            Type::App(ref a, ref b) => {
                a.free_type_vars(ctx, acc);
                b.free_type_vars(ctx, acc);
            }
            Type::Tuple(ts) => {
                for ty in ts {
                    ty.free_type_vars(ctx, acc);
                }
            }
            Type::Record(ref fields) => {
                for (_, field_ty) in fields {
                    field_ty.free_type_vars(ctx, acc);
                }
            }
            // Type::Overloaded(_, cands) => {
            //     for sch in cands {
            //         // sch.vars は束縛変数なので無視
            //         for c in &sch.constraints {
            //             for t in &c.params {
            //                 t.free_type_vars(ctx, acc);
            //             }
            //         }
            //         sch.target.free_type_vars(ctx, acc);
            //     }
            // }
            Type::Overloaded(_, _cands) => {
                todo!()
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
            Type::Con(c) => Type::con(c.clone()),

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
            // Type::Overloaded(name, cands) => {
            //     let mut new_cands = vec![];
            //     for sch in cands.iter() {
            //         // let vars = sch.vars.iter().map(|v| ctx.find(*v)).collect();
            //         let vars = sch.vars.clone();
            //         let constraints = sch.constraints.iter().map(|c| {
            //             let params = c.params.iter().map(|t| t.repr(ctx)).collect();
            //             Constraint { name: c.name.clone(), params }
            //         }).collect();
            //         let ty = &sch.target.repr(ctx);
            //         new_cands.push(TypeScheme::new(vars, constraints, ty.clone()));
            //     }
            //     Type::Overloaded(name.clone(), new_cands)
            // }
            Type::Overloaded(_name, _cands) => {
                todo!()
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
                    Type::Fun(_, _) | Type::App(_, _) => write!(f, "{} ({})", fun, arg),
                    _ => write!(f, "{} {}", fun, arg),
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
                                .map(|(k, v)| format!("{}: {}", k, v))
                                .collect();
                    write!(f, "@{{ {} }}", s.join(", "))
                }
            }
            Type::Overloaded(_name, _) => {
                write!(f, "<overloaded>")
            }
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct TypeVarId(pub usize);

impl fmt::Display for TypeVarId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "?{}", self.0)
    }
}

impl Type {
    pub fn score(&self) -> (usize, i64) {
        match self {
            Type::Var(_) => (0, -1),
            Type::Con(_) => (1, 0),
            Type::App(f, x) => {
                let (s1, g1) = f.score();
                let (s2, g2) = x.score();
                (s1+s2, g1+g2)
            }
            Type::Fun(a, b) => {
                let (s1, g1) = a.score();
                let (s2, g2) = b.score();
                (s1+s2, g1+g2)
            }
            Type::Tuple(es) => {
                let mut ret = (0, 0);
                for e in es.iter() {
                    let (s, g) = e.score();
                    ret.0 += s;
                    ret.1 += g;
                }
                ret
            }
            Type::Record(fields) => {
                let mut ret = (0, 0);
                for (_, e) in fields.iter() {
                    let (s, g) = e.score();
                    ret.0 += s;
                    ret.1 += g;
                }
                ret
            }
            Type::Overloaded(_, _) => {
                todo!()
            }
        }
    }
}
