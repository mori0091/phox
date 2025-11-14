use std::collections::HashMap;
use crate::resolve::*;
use super::*;

pub type TypeScheme = Scheme<Type>;

impl TypeScheme {
    pub fn from(raw: &RawTypeScheme, ctx: &mut TypeContext) -> Self {
        let mut var_map = HashMap::new();
        let mut vars = Vec::new();
        for v in raw.vars.iter() {
            let id = ctx.fresh_type_var_id();
            var_map.insert(v.clone(), id.clone());
            vars.push(id);
        }

        let constraints = raw
            .constraints
            .iter()
            .map(|c| resolve_raw_trait_head_local(ctx, c, &var_map))
            .collect();

        let target = resolve_raw_type(ctx, &raw.target, &var_map);

        TypeScheme::new(vars, constraints, target)
    }
}
