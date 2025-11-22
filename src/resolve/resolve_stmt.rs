use super::*;

// -------------------------------------------------------------
// === stmt ===
pub fn resolve_stmt(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    stmt: &mut Stmt,
) -> Result<(), TypeError> {
    match stmt {
        Stmt::Mod(name, items) => {
            let sub = &module.add_submod(name);
            phox.eval_mod(sub, "use ::prelude::*;").unwrap();
            if let Some(items) = items {
                phox.resolve_items(sub, items)?;
            }
            phox.eval_mod(module, &format!("use {name};")).unwrap();
            Ok(())
        }
        Stmt::Use(pathglob) => {
            resolve_stmt_use(phox, module, symbol_env, pathglob)
        }
        Stmt::Let(pat, expr) => {
            resolve_expr(phox, module, symbol_env, expr)?;
            resolve_pat(phox, module, symbol_env, pat)
        }
        Stmt::LetRec(pat, expr) => {
            resolve_pat(phox, module, symbol_env, pat)?;
            resolve_expr(phox, module, symbol_env, expr)
        }
    }
}
