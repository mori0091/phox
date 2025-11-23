use super::*;

// -------------------------------------------------------------
/// Make resolved top-level symbol.
pub fn make_top_level_symbol(
    phox: &mut PhoxEngine,
    module: &RefModule,
    name: &str,
) -> Result<Symbol, TypeError> {
    let symbol_env = &mut phox.get_symbol_env(module);
    make_symbol(phox, module, symbol_env, name)
}

// -------------------------------------------------------------
/// Make resolved local symbol.
pub fn make_symbol(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    name: &str,
) -> Result<Symbol, TypeError> {
    let mut symbol = Symbol::unresolved(name);
    resolve_symbol(phox, module, symbol_env, &mut symbol)?;
    Ok(symbol)
}
