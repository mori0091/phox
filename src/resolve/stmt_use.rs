use super::*;

pub fn resolve_stmt_use(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    pathglob: &PathGlob,
) -> Result<(), Error> {
    for (alias, path) in pathglob.flatten().iter() {
        match path.resolve(module, &phox.roots) {
            None => {
                return Err(Error::UnknownPath(path.clone()))
            }
            Some((m, None)) => {
                let p = m.borrow().path();
                module.borrow_mut().add_alias(&alias, &p)?;
            }
            Some((m, Some(rem))) => {
                if rem.len() > 1 {
                    return Err(Error::UnknownPath(rem.clone()))
                }
                match rem.first().unwrap() {
                    PathComponent::Wildcard => {
                        let other_symbol_env = phox.get_symbol_env(&m);
                        for (p, _sym) in other_symbol_env.clone_map().iter() {
                            let elem = p.first().cloned().unwrap(); // PathComponent::Name(name)
                            let alias = &elem.to_string();
                            let path = &m.borrow().path().concat(&[elem]);
                            module.borrow_mut().add_alias(alias, path)?;
                            make_symbol(phox, module, symbol_env, alias)?;
                        }
                        for (alias, p) in m.borrow().using.iter() {
                            let path = &m.borrow().path().concat_path(p);
                            module.borrow_mut().add_alias(&alias, path)?;
                            make_symbol(phox, module, symbol_env, &alias)?;
                        }
                    }
                    PathComponent::Name(name) => {
                        let other_symbol_env = phox.get_symbol_env(&m);
                        if let Some(_sym) = other_symbol_env.get(&rem) {
                            let path = &m.borrow().path().concat_path(&rem); // normalize to absolute path
                            module.borrow_mut().add_alias(&alias, path)?;
                            make_symbol(phox, module, symbol_env, &alias)?;
                        }
                        else if let Some(p) = m.borrow().using.get(name) {
                            let path = &m.borrow().path().concat_path(p);
                            module.borrow_mut().add_alias(&alias, path)?;
                            make_symbol(phox, module, symbol_env, &alias)?;
                        }
                        else {
                            return Err(Error::UnknownPath(rem.clone()))
                        }
                    }
                }
            }
        }
    }
    Ok(())
}
