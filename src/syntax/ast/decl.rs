mod raw;
pub use raw::*;

mod resolved;
pub use resolved::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Decl {
    Type(RawTypeDef),           // parsed, "raw" type decl.
    Trait(RawTrait),            // parsed, "raw" trait decl.
    Impl(RawImpl),              // parsed, "raw" impl decl.
}

use std::fmt;

impl fmt::Display for Decl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Decl::Type(raw) => write!(f, "{:?}", raw),
            Decl::Trait(raw) => write!(f, "{:?}", raw),
            Decl::Impl(raw) => write!(f, "{:?}", raw),
        }
    }
}
