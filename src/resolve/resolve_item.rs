use super::*;

// -------------------------------------------------------------
// === item ===
pub fn resolve_item(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    item: &mut Item,
) -> Result<(), TypeError> {
    match item {
        Item::Decl(decl) => {
            resolve_decl(phox, module, symbol_env, decl)
        }
        Item::Stmt(stmt) => {
            resolve_stmt(phox, module, symbol_env, stmt)
        }
        Item::Expr(expr) => {
            resolve_expr(phox, module, symbol_env, expr)
        }
    }
}
