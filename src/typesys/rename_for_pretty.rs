use indexmap::IndexMap;

use super::*;

pub type VarNameMap = IndexMap<Var, String>;

pub trait RenameForPretty {
    fn rename_var(&self, map: &mut VarNameMap) -> Self;
}
