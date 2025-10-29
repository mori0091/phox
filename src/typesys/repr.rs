use super::TypeContext;

pub trait Repr {
    fn repr(&self, ctx: &mut TypeContext) -> Self;
}
