use std::fmt;
use super::Scheme;

// ===== Types =====
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Var(TypeVarId),             // 型変数
    Fun(Box<Type>, Box<Type>),  // 関数型
    Con(String),                // 型構築子
    App(Box<Type>, Box<Type>),  // 型適用

    Tuple(Vec<Type>),
    Record(Vec<(String, Type)>),

    Overloaded(String, Vec<Scheme>),
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

impl Type {
    pub fn apply(&self, subst: &HashMap<TypeVarId, Type>) -> Type {
        match self {
            Type::Var(id) => subst.get(id).cloned().unwrap_or(Type::Var(*id)),
            Type::Con(name) => Type::Con(name.clone()),
            Type::Fun(t1, t2) => {
                Type::fun(t1.apply(subst), t2.apply(subst))
            }
            Type::App(t1, t2) => {
                Type::app(t1.apply(subst), t2.apply(subst))
            }
            Type::Tuple(ts) => {
                let ts2 = ts.iter().map(|t| t.apply(subst)).collect();
                Type::Tuple(ts2)
            }
            Type::Record(fields) => {
                let fields2 = fields.iter().map(|(name, t)| (name.clone(), t.apply(subst))).collect();
                Type::Record(fields2)
            }
            Type::Overloaded(_, _) => {
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
