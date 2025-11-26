use super::*;

// -------------------------------------------------------------
// === stmt ===
pub fn resolve_stmt(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    stmt: &mut Stmt,
) -> Result<(), Error> {
    match stmt {
        Stmt::Mod(name, items) => {
            let sub = &module.add_submod(name);
            if is_prelude_required_by(sub) {
                phox.eval_mod(sub, "use ::prelude::*;")?;
            }

            if let Some(items) = items {
                phox.eval_items(sub, items)?;
            }
            phox.eval_mod(module, &format!("use {name};"))?;
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

fn is_prelude_required_by(module: &RefModule) -> bool {
    if let Some(compo) = module.borrow().path().head() {
        if let PathComponent::Name(name) = compo {
            if name == "core" || name == "prelude" {
                return false
            }
        }
    }
    true
}
