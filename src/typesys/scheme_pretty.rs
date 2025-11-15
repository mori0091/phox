use std::collections::HashMap;
use super::TypeVarId;

pub trait SchemePretty {
    fn rename_type_var(&self, map: &mut HashMap<TypeVarId, String>) -> Self;
}
