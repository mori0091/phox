use std::fmt;
use super::*;

// ===== TypeVarId =====
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVarId(pub usize);

impl VarId for TypeVarId {
    fn from_usize(u: usize) -> Self {
        TypeVarId(u)
    }
    fn to_usize(self) -> usize {
        self.0
    }
}

impl fmt::Display for TypeVarId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "?{}", self.0)
    }
}
