use std::fmt;
use crate::typesys::{Type, TypeVarId, Scheme};

#[derive(Clone, Debug)]
pub enum TypeDecl {
    SumType {
        name: String,           // Type constructor name of the ADT type
        params: Vec<TypeVarId>, // Type variables of the ADT type
        variants: Vec<Variant>, // Constructor variants
    }
}

impl fmt::Display for TypeDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeDecl::SumType {name, params, variants} => {
                assert!(!variants.is_empty());
                let s = if params.is_empty() {
                    name.clone()
                } else {
                    format!("{} {}", name, params.iter()
                            .map(|id| format!("{}", id))
                            .collect::<Vec<_>>().join(" "))
                };
                let vs: Vec<_> = variants.iter().map(|v| v.to_string()).collect();
                write!(f, "type {} = {}", s, vs.join(" | "))
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Variant {
    Unit(String),                        // ex. `None, `Nil`,
    Tuple(String, Vec<Type>),            // ex. `Some a`, `Result a e`,
    // Record(String, Vec<(String, Type)>), // ex. `Point@{ x:a, y:a }`
}

impl fmt::Display for Variant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variant::Unit(name) => write!(f, "{}", name),
            Variant::Tuple(name, ts) => {
                assert!(!ts.is_empty());
                let s: Vec<_> = ts
                    .iter()
                    .map(|t| match t {
                        Type::Fun(_, _) | Type::App(_, _) => format!("({})", t),
                        _ => format!("{}", t)
                    })
                    .collect();
                write!(f, "{} {}", name, s.join(" "))
            }
            // Variant::Record(name, fields) => {
            //     if fields.is_empty() {
            //         write!(f, "{}@{{}}", name)
            //     }
            //     else {
            //         let s: Vec<String> = fields
            //             .iter()
            //             .map(|(k, v)| format!("{}: {}", k, v))
            //             .collect();
            //         write!(f, "{}@{{ {} }}", name, s.join(", "))
            //     }
            // }
        }
    }
}

impl Variant {
    pub fn name(&self) -> String {
        match self {
            // Variant::Unit(n)     |
            // Variant::Tuple(n, _) |
            // Variant::Record(n, _)
            //     => n.clone(),
            Variant::Unit(n) | Variant::Tuple(n, _) => n.clone(),
        }
    }
}

impl Variant {
    pub fn as_scheme(&self, type_name: &str, params: &[TypeVarId]) -> (String, Scheme) {
        // 型コンストラクタ適用: Option a, Result a b, ...
        let mut applied = Type::Con(type_name.to_string());
        for &p in params {
            applied = Type::App(Box::new(applied), Box::new(Type::Var(p)));
        }

        // コンストラクタの型を構築
        let ctor_type = match self {
            Variant::Unit(_) => applied,
            Variant::Tuple(_, elems) => {
                let mut t = applied;
                for arg in elems.iter().rev() {
                    t = Type::Fun(Box::new(arg.clone()), Box::new(t));
                }
                t
            }
            // Variant::Record(_, fields) => {
            //     // レコード型を1引数として扱う
            //     let record_ty = Type::Record(
            //         fields
            //             .iter()
            //             .map(|(fname, ty)| (fname.clone(), ty.clone()))
            //             .collect(),
            //     );
            //     Type::Fun(Box::new(record_ty), Box::new(applied))
            // }
        };

        // Scheme 化
        let scheme = Scheme::poly(params.to_vec(), ctor_type);

        // コンストラクタ名と Scheme を返す
        (self.name().to_string(), scheme)
    }
}
