use std::collections::HashMap;
use super::*;

pub trait RenameForPretty {
    fn rename_var(&self, map: &mut HashMap<Var, String>) -> Self;
}
