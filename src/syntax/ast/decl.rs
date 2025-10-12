use std::fmt;
use crate::typesys::Type;

#[derive(Clone, Debug)]
pub enum TypeDecl {
    SumType {
        name: String,           // Type constructor name of the ADT type
        params: Vec<String>,    // Type variables of the ADT type
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
                    format!("{} {}", name, params.join(" "))
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
    Record(String, Vec<(String, Type)>), // ex. `Point@{ x:a, y:a }`
}

impl fmt::Display for Variant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variant::Unit(name) => write!(f, "{}", name),
            Variant::Tuple(name, ts) => {
                assert!(!ts.is_empty());
                let s: Vec<_>
                    = ts.iter()
                        .map(|t| match t {
                            Type::Fun(_, _) | Type::App(_, _) => format!("({})", t),
                            _ => format!("{}", t)
                        })
                        .collect();
                write!(f, "{} {}", name, s.join(" "))
            }
            Variant::Record(name, fields) => {
                if fields.is_empty() {
                    write!(f, "{}@{{}}", name)
                }
                else {
                    let s: Vec<String>
                        = fields.iter()
                                .map(|(k, v)| format!("{}: {}", k, v))
                                .collect();
                    write!(f, "{}@{{ {} }}", name, s.join(", "))
                }
            }
        }
    }
}
