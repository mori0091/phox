// ===== Kinds =====
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Kind {
    Star,                        // *
    Fun(Box<Kind>, Box<Kind>),   // k1 -> k2
}
