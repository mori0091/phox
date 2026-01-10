use std::fmt;

mod raw;
pub use raw::*;

mod resolved;
pub use resolved::*;

use crate::typesys::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Decl {
    Type(RawTypeDef),           // parsed, "raw" type decl.
    Trait(RawTrait),            // parsed, "raw" trait decl.
    RawImpl(RawImpl),           // parsed, "raw" impl decl.
    NamedImpl(NamedImpl),       // resolved, "named" impl decl.
    SchImpl(Scheme<TypedImpl>), // resolved, "annotated" impl decl.
}

impl fmt::Display for Decl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Decl::Type(raw) => write!(f, "{:?}", raw),
            Decl::Trait(raw) => write!(f, "{:?}", raw),
            Decl::RawImpl(raw) => write!(f, "{:?}", raw),
            Decl::NamedImpl(named) => write!(f, "{:?}", named),
            Decl::SchImpl(scheme) => write!(f, "{:?}", scheme),
        }
    }
}
