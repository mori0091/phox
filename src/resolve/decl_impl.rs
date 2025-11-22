use super::*;

// -------------------------------------------------------------
// === impl ===
pub fn resolve_decl_impl(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    raw: &RawImpl,
) -> Result<(), TypeError> {
    let head_sch = resolve_impl_head_scheme(phox, module, symbol_env, raw)?;
    check_impl_comflict(phox, &head_sch)?;

    let mut members = Vec::new();
    for m in raw.members.iter() {
        let symbol = Symbol::trait_member(&m.name);
        let sch_tmpl = SchemeTemplate::new(
            resolve_impl_member_scheme(phox, module, &head_sch, m)?
        );
        let mut expr = m.expr.as_ref().clone();
        resolve_expr(phox, module, symbol_env, &mut expr)?;
        members.push((symbol, expr, sch_tmpl));
    };

    for (symbol, expr, sch_tmpl) in members {
        phox.impl_env
            .entry(head_sch.clone())
            .or_default()
            .insert(symbol.clone(), expr);
        phox.impl_member_env
            .entry(symbol)
            .or_default()
            .insert(sch_tmpl);
    }
    Ok(())
}

fn check_impl_comflict(
    phox: &mut PhoxEngine,
    impl_head_sch: &TraitScheme,
) -> Result<(), TypeError> {
    for (sch, _) in phox.impl_env.iter() {
        if sch.target.name != impl_head_sch.target.name { continue }
        if sch.target.score() != impl_head_sch.target.score() { continue }
        let mut ctx2 = phox.ctx.clone();
        let mut same = true;
        for (t1, t2) in sch.target.params.iter().zip(impl_head_sch.target.params.iter()) {
            if ctx2.unify(t1, t2).is_err() {
                same = false;
                break;
            }
        }
        if same {
            return Err(TypeError::ConflictImpl {
                it: impl_head_sch.target.clone(),
                other: sch.target.clone(),
            });
        }
    };
    Ok(())
}

// -------------------------------------------------------------
// === impl head ===
pub fn resolve_impl_head(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    raw: &RawTraitHead,
    param_map: &HashMap<String, TypeVarId>,
) -> Result<TraitHead, TypeError> {
    let mut symbol = raw.name.clone();
    resolve_symbol(phox, module, symbol_env, &mut symbol)?;
    let mut params = Vec::new();
    for t in raw.params.iter() {
        let ty = resolve_raw_type(phox, module, symbol_env, t, &mut param_map.clone())?;
        params.push(ty);
    }
    Ok(TraitHead { name: symbol, params })
}

// -------------------------------------------------------------
// === impl head scheme ===
fn resolve_impl_head_scheme(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    raw: &RawImpl,
) -> Result<Scheme<TraitHead>, TypeError> {
    let mut param_map = HashMap::new();
    for p in raw.params.iter() {
        if let RawType::VarName(name) = p {
            param_map
                .entry(name.clone())
                .or_insert_with(|| {
                    phox.ctx.fresh_type_var_id()
                });
        }
    }
    let impl_head = resolve_impl_head(
        phox,
        module,
        symbol_env,
        &raw.head(),
        &param_map
    )?;

    let icx = &phox.get_infer_ctx(module);
    let impl_head_sch = generalize(&mut phox.ctx, icx, &impl_head);

    Ok(impl_head_sch)
}

// -------------------------------------------------------------
// === impl member type scheme ===
fn resolve_impl_member_scheme(
    phox: &mut PhoxEngine,
    module: &RefModule,
    impl_head_sch: &Scheme<TraitHead>,
    raw_member: &RawImplMember,
) -> Result<TypeScheme, TypeError> {
    let trait_scheme_tmpls = phox
        .get_infer_ctx(module)
        .get_trait_member_schemes(&Symbol::trait_member(&raw_member.name))
        .ok_or(TypeError::UnknownTraitMember(raw_member.name.clone()))?;

    // -------------------------------------------------
    if trait_scheme_tmpls.is_empty() {
        return Err(TypeError::UnknownTraitMember(raw_member.name.clone()));
    }

    // -------------------------------------------------
    let trait_scheme_tmpl = trait_scheme_tmpls
        .iter()
        .find(|tmpl| tmpl.scheme_ref().constraints[0].name == impl_head_sch.target.name)
        .ok_or(TypeError::UnknownTrait(impl_head_sch.target.name.clone()))?;

    // -------------------------------------------------
    let trait_head = &trait_scheme_tmpl.scheme_ref().constraints[0];
    if impl_head_sch.target.params.len() != trait_head.params.len() {
        return Err(TypeError::ArityMismatch {
            trait_name: trait_head.name.clone(),
            member: raw_member.name.clone(),
            expected: trait_head.params.len(),
            actual: impl_head_sch.target.params.len()
        });
    }

    // -------------------------------------------------
    let mut subst: HashMap<TypeVarId, Type> = HashMap::new();
    for (t1, t2) in trait_head.params.iter().zip(impl_head_sch.target.params.iter()) {
        if let Type::Var(id) = t1 {
            subst.insert(*id, t2.clone());
        }
    }
    let ty = trait_scheme_tmpl
        .scheme_ref()
        .target
        .apply_subst(&subst);

    Ok(TypeScheme {
        vars: impl_head_sch.vars.clone(),
        constraints: vec![impl_head_sch.target.clone()],
        target: ty,
    })
}
