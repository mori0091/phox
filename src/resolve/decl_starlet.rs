use super::*;

// -------------------------------------------------------------
// === *let ===
pub fn resolve_decl_starlet(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    raw: &RawStarlet,
) -> Result<NamedStarlet, Error> {
    let name = make_symbol(phox, module, symbol_env, &raw.name)?;
    // let mut name = Symbol::unresolved(&raw.name);
    // resolve_symbol(phox, module, symbol_env, &mut name)?;
    let mut expr = raw.expr.as_ref().clone();
    let mut param_map = TyParMap::new();
    resolve_expr(phox, module, symbol_env, &mut param_map, &mut expr)?;

    Ok(NamedStarlet { name, expr })
}
