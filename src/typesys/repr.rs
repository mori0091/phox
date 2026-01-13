use super::TypeContext;

/// Representative form (canonical form)
pub trait Repr {
    /// Convert to representative form (i.e., the form after applying the most
    /// general unifier substitution)
    fn repr(&self, ctx: &mut TypeContext) -> Self;
}
