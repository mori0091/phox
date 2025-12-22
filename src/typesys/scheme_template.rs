use super::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SchemeTemplate<T> {
    scheme: Scheme<T>,
}

impl <T> SchemeTemplate<T> {
    pub fn new(resolved: Scheme<T>) -> Self {
        Self { scheme: resolved }
    }
    pub fn scheme_ref(&self) -> &Scheme<T> {
        &self.scheme
    }
}

impl <T: ApplySubst> SchemeTemplate<T> {
    pub fn fresh_copy(&self, ctx: &mut TypeContext) -> Scheme<T> {
        self.scheme.fresh_copy(ctx)
    }
}
