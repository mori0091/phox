use crate::api::loader::*;
use crate::api::parse;

use super::*;

// -------------------------------------------------------------
// === decl ===
pub fn resolve_decl(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    decl: &mut Decl,
) -> Result<(), Error> {
    match decl {
        Decl::Mod(name, opt_items) => {
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
            // Overwrite this `Decl::Mod` node.
            *opt_items = Some(items);

            Ok(())
        }
        Decl::Use(pathglob) => {
            resolve_stmt_use(phox, module, symbol_env, pathglob)
        }

        Decl::Type(raw) => {
            resolve_decl_type_def(phox, module, symbol_env, raw)
        }
        Decl::Trait(raw) => {
            resolve_decl_trait(phox, module, symbol_env, raw)
        }

        Decl::RawImpl(raw) => {
            let named = resolve_decl_impl(phox, module, symbol_env, raw)?;
            *decl = Decl::NamedImpl(named);
            Ok(())
        }
        Decl::NamedImpl(_named) => {
            Ok(())
        }
        Decl::SchImpl(_) => {
            Ok(())
        }

        Decl::RawStarlet(raw) => {
            let named = resolve_decl_starlet(phox, module, symbol_env, raw)?;
            *decl = Decl::NamedStarlet(named);
            Ok(())
        }
        Decl::NamedStarlet(_named) => {
            Ok(())
        }
        Decl::SchStarlet(_) => {
            Ok(())
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
