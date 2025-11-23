use super::*;

// -------------------------------------------------------------
#[derive(Clone, Debug)]
pub struct Impl {
    pub head_sch: Scheme<TraitHead>,
    pub members: Vec<ImplMember>,
}

// -------------------------------------------------------------
#[derive(Clone, Debug)]
pub struct ImplMember {
    /// Shared/Non-unique member's symbol (i.e. `Symbol::trait_member(name)`)
    pub symbol: Symbol,
    /// An expression whose name is resolved but not type-checked.
    pub expr: Expr,
    /// A template for the type scheme the expression must satisfy.
    pub sch_tmpl: SchemeTemplate<Type>,
}
