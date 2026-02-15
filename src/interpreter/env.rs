use indexmap::IndexMap;

use super::Value;
use crate::module::*;
use crate::collection::RefMap;

pub type Binding = IndexMap<Symbol, Value>;
pub type ValueEnv = RefMap<Symbol, Value>;
