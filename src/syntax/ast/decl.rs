use std::fmt;

mod raw;
pub use raw::*;

mod resolved;
pub use resolved::*;

use crate::module::*;
use crate::typesys::*;
use super::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Decl {
    Mod(String, Option<Vec<Item>>), // `mod bar;`
    Use(PathGlob),              // `use ::foo::bar;`

    Type(RawTypeDef),           // parsed, "raw" type decl.
    Trait(RawTrait),            // parsed, "raw" trait decl.

    RawImpl(RawImpl),           // parsed, "raw" impl decl.
    NamedImpl(NamedImpl),       // resolved, "named" impl decl.
    SchImpl(Scheme<TypedImpl>), // resolved, "annotated" impl decl.

    RawStarlet(RawStarlet),           // parsed, "raw" *let decl.
    NamedStarlet(NamedStarlet),       // resolved, "named" *let decl.
    SchStarlet(Scheme<TypedStarlet>), // resolved, "annotated" *let decl.
}

impl fmt::Display for Decl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Decl::Mod(m, items) => {
                if let Some(items) = items {
                    let s: Vec<String> = items.iter()
                        .map(|item| format!("  {};\n", item))
                        .collect();
                    write!(f, "mod {} {{\n{}\n}}", m, s.join("\n"))
                }
                else {
                    write!(f, "mod {}", m)
                }
            },
            Decl::Use(p)       => write!(f, "use {}", p),

            Decl::Type(raw) => write!(f, "{:?}", raw),
            Decl::Trait(raw) => write!(f, "{:?}", raw),

            Decl::RawImpl(raw) => write!(f, "{:?}", raw),
            Decl::NamedImpl(named) => write!(f, "{:?}", named),
            Decl::SchImpl(scheme) => write!(f, "{:?}", scheme),

            Decl::RawStarlet(raw) => write!(f, "{:?}", raw),
            Decl::NamedStarlet(named) => write!(f, "{:?}", named),
            Decl::SchStarlet(scheme) => write!(f, "{:?}", scheme),
        }
    }
}
