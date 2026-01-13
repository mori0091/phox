use super::*;

// -------------------------------------------------------------
// === item ===
pub fn resolve_item(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    param_map: &mut HashMap<String, TypeVarId>,
    item: &mut Item,
) -> Result<(), Error> {
    match &mut item.body {
        ItemBody::Decl(decl) => {
            resolve_decl(phox, module, symbol_env, decl)
        }
        ItemBody::Stmt(stmt) => {
            resolve_stmt(phox, module, symbol_env, param_map, stmt)
        }
        ItemBody::Expr(expr) => {
            resolve_expr(phox, module, symbol_env, param_map, expr)
        }
    }
}
