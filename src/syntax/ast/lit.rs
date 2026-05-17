use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Lit {
    Unit,        // ()
    Bool(bool),  // true / false
    Int(i64),    // 整数リテラル
    U8(u8),
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lit::Unit => write!(f, "()"),
            Lit::Bool(b) => write!(f, "{}", b),
            Lit::Int(i) => write!(f, "{}", i),
            Lit::U8(u) => write!(f, "{}", u),
        }
    }
}
