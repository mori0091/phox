use super::*;

// -------------------------------------------------------------
// === pat ===
pub fn resolve_pat(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    pat: &mut Pat,
) -> Result<(), Error> {
    match pat {
        Pat::Wildcard | Pat::Lit(_) => Ok(()),
        Pat::Var(symbol) => {
            resolve_symbol(phox, module, symbol_env, symbol)
        }
        Pat::Con(symbol, args) => {
            resolve_symbol(phox, module, symbol_env, symbol)?;
            for p in args.iter_mut() {
                resolve_pat(phox, module, symbol_env, p)?;
            }
            Ok(())
        }
        Pat::Tuple(ps) => {
            for p in ps.iter_mut() {
                resolve_pat(phox, module, symbol_env, p)?;
            }
            Ok(())
        }
        Pat::Record(fields) => {
            for (_, p) in fields.iter_mut() {
                resolve_pat(phox, module, symbol_env, p)?;
            }
            Ok(())
        }
    }
}
