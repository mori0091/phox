use crate::api::loader::*;
use crate::api::parse;

use super::*;

// -------------------------------------------------------------
// === stmt ===
pub fn resolve_stmt(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    param_map: &mut HashMap<String, TypeVarId>,
    stmt: &mut Stmt,
) -> Result<(), Error> {
    match stmt {
        Stmt::Mod(name, opt_items) => {
            let sub = &module.add_submod(&name);
            if is_prelude_required_by(sub) {
                let mut xs = parse("use ::prelude::*;").unwrap();
                for x in xs.iter_mut() {
                    phox.resolve_item(sub, x)?;
                }
            }
            let mut items = Vec::new();
            if let Some(xs) = opt_items {
                items.extend(xs.clone());
            }
            else {
                let (file, src) = load_module_src(&sub.borrow().path())?;
                let xs = parse(&src).map_err(|e| {
                    Error::Message(format!("parse error `{}` {:?}", file.display(), e))
                })?;
                items.extend(xs);
            }
            // -------------------------------------------------
            phox.run_items(sub, &mut items)?;

            // -------------------------------------------------
            // `use {name};` in the current module.
            let mut xs = parse(&format!("use {name};")).unwrap();
            for x in xs.iter_mut() {
                phox.resolve_item(module, x)?;
            }

            // -------------------------------------------------
            // Overwrite this `Stmt::Mod` node.
            *opt_items = Some(items);

            Ok(())
        }
        Stmt::Use(pathglob) => {
            resolve_stmt_use(phox, module, symbol_env, pathglob)
        }
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

fn is_prelude_required_by(module: &RefModule) -> bool {
    if let Some(compo) = module.borrow().path().first() {
        if let PathComponent::Name(name) = compo {
            if name == "core" || name == "prelude" {
                return false
            }
        }
    }
    true
}
