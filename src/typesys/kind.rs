// ===== Kinds =====
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Kind {
    Fun(Box<Kind>, Box<Kind>),   // κ1 -> κ2
    Type,                        // τ
    // Row,                         // ρ
    // Nat,                         // ν
}
