use crate::module::*;
use crate::typesys::*;

#[derive(Clone, Debug)]
pub enum TypeDecl {
    SumType {
        name: Symbol,           // Type constructor name of the ADT type
        params: Vec<TypeVarId>, // Type variables of the ADT type
        variants: Vec<Variant>, // Constructor variants
    }
}

#[derive(Clone, Debug)]
pub enum Variant {
    Unit(Symbol),               // ex. `None, `Nil`,
    Tuple(Symbol, Vec<Type>),   // ex. `Some a`, `Result a e`,
}

impl Variant {
    pub fn name(&self) -> Symbol {
        match self {
            Variant::Unit(n) | Variant::Tuple(n, _) => n.clone(),
        }
    }
    pub fn arity(&self) -> usize {
        match self {
            Variant::Unit(_) => 0,
            Variant::Tuple(_, ts) => ts.len(),
        }
    }
}

impl Variant {
    pub fn as_scheme(&self, type_name: &Symbol, params: &[TypeVarId]) -> TypeScheme {
        // 型コンストラクタ適用: Option a, Result a b, ...
        let mut applied = Type::Con(type_name.clone());
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
        };

        TypeScheme::poly(params.to_vec(), ctor_type)
    }
}

// use std::fmt;

// impl fmt::Display for TypeDecl {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             TypeDecl::SumType {name, params, variants} => {
//                 assert!(!variants.is_empty());
//                 let s = if params.is_empty() {
//                     name.to_string()
//                 } else {
//                     format!("{} {}", name.to_string(), params.iter()
//                             .map(|id| format!("{}", id))
//                             .collect::<Vec<_>>().join(" "))
//                 };
//                 let vs: Vec<_> = variants.iter().map(|v| v.to_string()).collect();
//                 write!(f, "type {} = {}", s, vs.join(" | "))
//             }
//         }
//     }
// }

// impl fmt::Display for Variant {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Variant::Unit(name) => write!(f, "{}", name),
//             Variant::Tuple(name, ts) => {
//                 assert!(!ts.is_empty());
//                 let s: Vec<_> = ts
//                     .iter()
//                     .map(|t| match t {
//                         Type::Fun(_, _) | Type::App(_, _) => format!("({})", t),
//                         _ => format!("{}", t)
//                     })
//                     .collect();
//                 write!(f, "{} {}", name, s.join(" "))
//             }
//         }
//     }
// }
