use super::{Item, RawTypeDecl};

pub type Program = Vec<TopLevel>;

pub enum TopLevel {
    TypeDecl(RawTypeDecl),
    Item(Item),
}
