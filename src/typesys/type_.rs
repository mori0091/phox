use std::fmt;

// ===== Types =====
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Var(TypeVarId),             // 型変数
    Fun(Box<Type>, Box<Type>),  // 関数型
    Con(String),                // 型構築子
    App(Box<Type>, Box<Type>),  // 型適用

    Tuple(Vec<Type>),
    Record(Vec<(String, Type)>),

    // Struct(String, Vec<(String, Type)>),
}

impl Type {
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
            // Type::Struct(name, fields) => {
            //     if fields.is_empty() {
            //         write!(f, "{}@{{}}", name)
            //     }
            //     else {
            //         let s: Vec<String>
            //             = fields.iter()
            //                     .map(|(k, v)| format!("{}: {}", k, v))
            //                     .collect();
            //         write!(f, "{}@{{ {} }}", name, s.join(", "))
            //     }
            // }
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
