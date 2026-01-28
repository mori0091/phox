use std::collections::HashMap;
use super::*;

// -------------------------------------------------------------
// === impl ===
pub fn resolve_decl_impl(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    raw: &RawImpl,
) -> Result<NamedImpl, Error> {
    let mut param_map = HashMap::new();
    let head = resolve_impl_head(phox, module, symbol_env, &mut param_map, &raw.head())?;
    check_impl_conflict(phox, &head)?;

    let mut members = Vec::new();
    for RawImplMember {name, expr} in raw.members.iter() {
        let sym = Symbol::trait_member(name);
        let mut expr = expr.as_ref().clone();
        resolve_expr(phox, module, symbol_env, &mut param_map, &mut expr)?;
        members.push((sym, expr));
    };

    Ok(NamedImpl { head, members })
}

fn check_impl_conflict(
    phox: &mut PhoxEngine,
    impl_head: &TraitHead,
) -> Result<(), Error> {
    for tmpl in phox.impl_env.iter() {
        let mut ctx2 = phox.ctx.clone();
        let sch = tmpl.fresh_copy(&mut ctx2.ty);
        let head = &sch.target.head;
        if head.name != impl_head.name { continue }
        let mut same = true;
        for (t1, t2) in head.params.iter().zip(impl_head.params.iter()) {
            if ctx2.ty.unify(t1, t2).is_err() {
                same = false;
                break;
            }
        }
        if same {
            return Err(Error::ConflictImpl { it: impl_head.clone() });
        }
    };
    Ok(())
}

// -------------------------------------------------------------
// === impl head ===
/// \NOTE:
/// - To resolve the head of an `impl` declaration:
///   You must pass an empty `param_map`.
///   This function updates it, and it is used by the caller.
/// - When resolving a trait record expression like `@{T a}`:
///   You must pass an empty or non-empty `param_map` initialized within the context.
///   This function updates it, and it is used by the caller and shared throughout the context.
pub fn resolve_impl_head(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    param_map: &mut HashMap<String, TypeVarId>,
    raw: &RawTraitHead,
) -> Result<TraitHead, Error> {
    let mut symbol = raw.name.clone();
    resolve_symbol(phox, module, symbol_env, &mut symbol)?;
    let mut params = Vec::new();
    for t in raw.params.iter() {
        let ty = resolve_raw_type(phox, module, symbol_env, t, param_map)?;
        params.push(ty);
    }
    Ok(TraitHead { name: symbol, params })
}
