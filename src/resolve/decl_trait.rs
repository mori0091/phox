use std::collections::BTreeSet;

use super::*;

// -------------------------------------------------------------
// === trait ===
pub fn resolve_decl_trait(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    raw: &RawTrait,
) -> Result<(), Error> {
    let trait_symbol = make_symbol(phox, module, symbol_env, &raw.name)?;

    let mut vars = Vec::new();          // [id]
    let mut param_map = TyParMap::new(); // {"a": id}
    let head = {
        let mut params = Vec::new(); // [Type::Var(id)]
        for p in raw.params.iter() {
            let ty_var_id = phox.ctx.ty.fresh_var_id();
            vars.push(Var::Ty(ty_var_id.clone()));
            param_map.insert(p.to_string(), ty_var_id);
            let ty_var = Type::Var(ty_var_id);
            params.push(ty_var);
        }
        TraitHead { name: trait_symbol, params }
    };

    let mut member_schemes = Vec::new();
    for member in raw.members.iter() {
        let symbol = Symbol::trait_member(&member.name);
        let sch_tmpl = SchemeTemplate::new(
            TypeScheme {
                vars: vars.clone(),
                constraints: ConstraintSet {
                    primary: Some(Box::new(head.clone())),
                    requires: BTreeSet::new(),
                },
                target: resolve_raw_type(
                    phox,
                    module,
                    symbol_env,
                    &member.ty,
                    &mut param_map.clone()
                )?,
            }
        );
        member_schemes.push((symbol, sch_tmpl));
    }

    // ---------------------------------------------------------
    // register trait name -> member names
    let trait_name = raw.name.clone();
    let member_names = raw.members.iter().map(|m| m.name.clone()).collect::<Vec<_>>();
    module.borrow_mut().trait_members.insert(trait_name, member_names);

    // register trait member's scheme
    for (symbol, sch_tmpl) in member_schemes {
        phox.get_infer_ctx(module).put_trait_member_scheme(symbol, sch_tmpl);
    }

    Ok(())
}
