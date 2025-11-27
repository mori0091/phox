use super::*;

// -------------------------------------------------------------
// === resolve symbol ===
pub fn resolve_symbol(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    symbol: &mut Symbol,
) -> Result<(), Error> {
    // Is extern/global symbol?
    if let Symbol::Local(s) = symbol.clone() {
        if s.starts_with("::") {
            let ps = s.trim_start_matches("::")
                      .split("::")
                      .map(|s| PathComponent::Name(s.to_string()))
                      .collect::<Vec<_>>();
            *symbol = Symbol::Unresolved(Path::Absolute(ps));
        }
    }
    if let Symbol::Unresolved(path) = symbol {
        let path = path.clone();
        let resolved = match path {
            Path::Relative(_) => {
                resolve_symbol_relative(phox, module, symbol_env, path)?
            }
            Path::Absolute(_) => {
                resolve_symbol_absolute(phox, module, symbol_env, path)?
            }
        };
        *symbol = resolved;
    }
    Ok(())
}

// -------------------------------------------------------------
fn resolve_symbol_relative(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    path: Path,
) -> Result<Symbol, Error> {
    if let Some(s) = symbol_env.get(&path) {
        return Ok(s.clone())
    }

    // Is an alias contained in the path?
    let tmp_absolute_path = module.borrow().resolve_alias(&path);
    if let Some(alias_path) = tmp_absolute_path {
        let mut sym = Symbol::Unresolved(alias_path);
        resolve_symbol(phox, module, symbol_env, &mut sym)?;
        return Ok(sym)
    }

    // Otherwise, module top-level symbol, trait member, or local symbol
    let name = path.to_string();
    // Is a known trait member's name?
    if module.borrow().trait_members.values().any(|v| v.contains(&name)) {
        let sym = Symbol::trait_member(&name);
        return Ok(sym)
    }
    // Is local?
    else if symbol_env.is_local() {
        let sym = Symbol::local(&name);
        symbol_env.insert(path.clone(), sym.clone());
        return Ok(sym)
    }
    // module top-level symbol
    else {
        let sym = Symbol::Local(module.borrow().path().concat_path(&path).pretty());
        symbol_env.insert(path.clone(), sym.clone());
        return Ok(sym)
    }
}

// -------------------------------------------------------------
fn resolve_symbol_absolute(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    path: Path,
) -> Result<Symbol, Error> {
    if let Some(global_sym) = phox.get_extern_symbol_env(module).get(&path) {
        return Ok(global_sym)
    }

    // and then register target into corresponding tables.
    // - `mod` sub-module             -> module.submods \NOTE Symbols shall not be assigned to the module itself.
    // - `type` type constructor      -> module.icx.kind_env
    // - `trait` trait name           -> module.trait_members
    //   - `trait` member type scheme -> module.icx.trait_member_env
    //   - `impl` member type scheme  -> phox.impl_member_env \TODO move to module scope
    //   - `impl` expr                -> phox.impl_env        \TODO move to module scope
    // - top-level var type scheme    -> module.icx.type_env
    //   - top-level var expr         -> module.value_env
    match path.resolve(module, &phox.roots) {
        Some((m, None)) => {
            // NOTE:
            // A path specifying `module` itself is permitted only in `mod`/`use` statements.
            // So do not bind the symbol to any local/module environment.
            let extern_sym = Symbol::Local(m.borrow().path().pretty());
            return Ok(extern_sym);
        }
        Some((m, Some(rem))) if rem.len() == 1 => {
            if let Some(target_sym) = phox.get_symbol_env(&m).get(&rem) {
                let extern_sym = target_sym.clone(); // <- \NOTE may be an "extern" symbol?

                // is Data constructor or Variable ?
                if let Some(ty_sch) = phox.get_infer_ctx(&m).get_type_scheme(&target_sym) {
                    phox.get_infer_ctx(module).put_type_scheme(extern_sym.clone(), ty_sch);
                    if let Some(val) = phox.get_value_env(&m).get(&target_sym) {
                        phox.get_value_env(module).insert(extern_sym.clone(), val);
                    }

                    phox.get_extern_symbol_env(module).insert(path, extern_sym.clone());
                    return Ok(extern_sym)
                }

                // is Trait name ?
                if let Some(xs) = m.borrow().trait_members.get(&rem.to_string()) {
                    module.borrow_mut().trait_members.insert(rem.to_string(), xs.clone());
                    for member_name in xs.iter() {
                        let member_sym = Symbol::trait_member(member_name);
                        if let Some(member_schemes) = phox.get_infer_ctx(&m).get_trait_member_schemes(&member_sym) {
                            phox.get_infer_ctx(module).extend_trait_member_schemes(&member_sym, member_schemes);
                        }
                        // \TODO import `impl_member_env`
                    }

                    phox.get_extern_symbol_env(module).insert(path, extern_sym.clone());
                    return Ok(extern_sym)
                }

                // is Type constructor ?
                if let Some(k) = phox.get_infer_ctx(&m).get_kind(&target_sym) {
                    phox.get_infer_ctx(module).put_kind(extern_sym.clone(), k.clone());

                    phox.get_extern_symbol_env(module).insert(path, extern_sym.clone());
                    return Ok(extern_sym)
                }
            }
            // is alias ?
            else if let Some(p) = m.borrow().using.get(&rem.to_string()) {
                let mut extern_sym = Symbol::Unresolved(m.borrow().path().concat_path(p));
                resolve_symbol(phox, module, symbol_env, &mut extern_sym)?;
                phox.get_extern_symbol_env(module).insert(path, extern_sym.clone());
                return Ok(extern_sym)
            }
            // return Err(Error::UnknownPath(path))
        }
        _ => {
            // return Err(Error::UnknownPath(path))
        }
    }

    Err(Error::UnknownPath(path))
}
