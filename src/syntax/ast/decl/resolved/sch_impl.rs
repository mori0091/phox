use super::*;

// ----------------------------------------------
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SchImpl {
    pub head_sch: Scheme<TraitHead>,
    pub members: Vec<ImplMember>,
}

// ----------------------------------------------
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ImplMember {
    pub name: Symbol,           // Symbol::Local(name)
    pub expr: Expr,
    pub sch_tmpl: SchemeTemplate<Type>,
}
