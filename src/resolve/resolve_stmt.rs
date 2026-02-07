use super::*;

// -------------------------------------------------------------
// === stmt ===
pub fn resolve_stmt(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    param_map: &mut TyParMap,
    stmt: &mut Stmt,
) -> Result<(), Error> {
    match stmt {
        Stmt::Let(pat, expr) => {
            resolve_expr(phox, module, symbol_env, param_map, expr)?;
            resolve_pat(phox, module, symbol_env, pat)
        }
        Stmt::LetRec(pat, expr) => {
            resolve_pat(phox, module, symbol_env, pat)?;
            resolve_expr(phox, module, symbol_env, param_map, expr)
        }
    }
}
