use std::collections::HashMap;

use crate::typesys::*;
use crate::syntax::ast::*;
use crate::module::*;
use crate::api::PhoxEngine;

// -------------------------------------------------------------
// === item ===
pub fn resolve_item(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    item: &mut Item,
) -> Result<(), TypeError> {
    match item {
        Item::RawTraitDecl(raw) => {
            register_trait(phox, module, symbol_env, raw)
        }
        Item::RawImplDecl(raw) => {
            register_impl(phox, module, symbol_env, raw)
        }
        Item::RawTypeDecl(raw) => {
            register_type_decl(phox, module, symbol_env, raw)
        }
        Item::Stmt(stmt) => {
            resolve_stmt(phox, module, symbol_env, stmt)
        }
        Item::Expr(expr) => {
            resolve_expr(phox, module, symbol_env, expr)
        }
    }
}

// -------------------------------------------------------------
// === trait ===
fn register_trait(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    raw: &RawTraitDecl,
) -> Result<(), TypeError> {
    {
        // register trait name -> member names
        let name = raw.name.clone();
        let member_names = raw
            .members
            .iter()
            .map(|m| m.name.clone())
            .collect::<Vec<_>>();
        module.borrow_mut()
              .trait_members
              .insert(name, member_names);
    }

    let trait_name = make_symbol(phox, module, symbol_env, &raw.name)?;

    let mut vars = Vec::new();          // [id]
    let mut param_map = HashMap::new(); // {"a": id}
    let mut params = Vec::new();        // [Type::Var(id)]
    for p in raw.params.iter() {
        let ty_var_id = phox.ctx.fresh_type_var_id();
        vars.push(ty_var_id.clone());
        param_map.insert(p.to_string(), ty_var_id);
        let ty_var = Type::Var(ty_var_id);
        params.push(ty_var);
    }
    let head = TraitHead {
        name: trait_name,
        params: params.clone(),
    };

    for member in raw.members.iter() {
        let target = resolve_raw_type(phox, module, symbol_env, &member.ty, &mut param_map.clone())?;
        let sch = TypeScheme {
            vars: vars.clone(),
            constraints: vec![head.clone()],
            target,
        };
        let symbol = Symbol::local(&member.name); // <- \NOTE This must NOT be a unique symbol.
        phox.get_infer_ctx(module)
            .put_trait_member_scheme(symbol, SchemeTemplate::new(sch));
    }
    Ok(())
}

// -------------------------------------------------------------
// === impl ===
fn register_impl(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    raw: &RawImplDecl,
) -> Result<(), TypeError> {
    let impl_head = {
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
        resolve_raw_trait_head(phox, module, symbol_env, &raw.head(), &param_map)
    }?;

    let icx = &phox.get_infer_ctx(module);
    let impl_head_sch = generalize(&mut phox.ctx, icx, &impl_head);

    // ----------------------------------------------------
    check_impl_comflict(phox, &impl_head_sch)?;

    // ----------------------------------------------------
    let mut impls = HashMap::new();

    for raw_member in raw.members.iter() {
        let sym = Symbol::local(&raw_member.name);
        let impl_member_sch = {
            // -------------------------------------------------
            let trait_scheme_tmpls = phox
                .get_infer_ctx(module)
                .get_trait_member_schemes(&sym)
                .ok_or(TypeError::UnknownTraitMember(raw_member.name.clone()))?;

            // -------------------------------------------------
            if trait_scheme_tmpls.is_empty() {
                return Err(TypeError::UnknownTraitMember(raw_member.name.clone()));
            }

            // -------------------------------------------------
            let trait_scheme_tmpl = trait_scheme_tmpls
                .iter()
                .find(|tmpl| tmpl.scheme_ref().constraints[0].name == impl_head.name)
                .ok_or(TypeError::UnknownTrait(raw.name.clone()))?;

            // -------------------------------------------------
            let trait_head = &trait_scheme_tmpl.scheme_ref().constraints[0];
            if impl_head.params.len() != trait_head.params.len() {
                return Err(TypeError::ArityMismatch {
                    trait_name: trait_head.name.clone(),
                    member: raw_member.name.clone(),
                    expected: trait_head.params.len(),
                    actual: impl_head.params.len()
                });
            }

            // -------------------------------------------------
            let mut subst: HashMap<TypeVarId, Type> = HashMap::new();
            for (t1, t2) in trait_head.params.iter().zip(impl_head.params.iter()) {
                if let Type::Var(id) = t1 {
                    subst.insert(*id, t2.clone());
                }
            }
            let ty = trait_scheme_tmpl
                .scheme_ref()
                .target
                .apply_subst(&subst);

            TypeScheme {
                vars: impl_head_sch.vars.clone(),
                constraints: vec![impl_head.clone()],
                target: ty,
            }
        };

        phox.impl_member_env
            .entry(sym.clone())
            .or_default()
            .insert(SchemeTemplate::new(impl_member_sch));

        let mut expr = raw_member.expr.as_ref().clone();
        resolve_expr(phox, module, symbol_env, &mut expr)?;
        impls.insert(sym, expr);
    };
    phox.impl_env.insert(impl_head_sch, impls);

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
            for (alias, path) in pathglob.flatten().iter() {
                match path.resolve(module, &phox.roots) {
                    None => {
                        return Err(TypeError::UnknownPath(path.clone()))
                    }
                    Some((m, None)) => {
                        let p = m.borrow().path();
                        module.borrow_mut().add_alias(&alias, &p)?;
                    }
                    Some((m, Some(rem))) => {
                        if rem.len() > 1 {
                            return Err(TypeError::UnknownPath(rem.clone()))
                        }
                        match rem.head().unwrap() {
                            PathComponent::Wildcard => {
                                let other_symbol_env = phox.get_symbol_env(&m);
                                for (p, _sym) in other_symbol_env.clone_map().iter() {
                                    let elem = p.head().unwrap(); // PathComponent::Name(name)
                                    let name = elem.to_string();
                                    let alias = &name;
                                    let path = &m.borrow().path().concat(&[elem]);
                                    module.borrow_mut().add_alias(alias, path)?;
                                    make_symbol(phox, module, symbol_env, &alias)?;
                                }
                            }
                            PathComponent::Name(_name) => {
                                let other_symbol_env = phox.get_symbol_env(&m);
                                if let Some(_sym) = other_symbol_env.get(&rem) {
                                    module.borrow_mut().add_alias(&alias, path)?;
                                    make_symbol(phox, module, symbol_env, &alias)?;
                                } else {
                                    return Err(TypeError::UnknownPath(rem.clone()))
                                }
                            }
                        }
                    }
                }
            }
            Ok(())
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

// -------------------------------------------------------------
// === pat ===
fn resolve_pat(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    pat: &mut Pat,
) -> Result<(), TypeError> {
    match pat {
        Pat::Var(symbol) => {
            resolve_symbol(phox, module, symbol_env, symbol)?;
            Ok(())
        }
        Pat::Con(symbol, args) => {
            resolve_symbol(phox, module, symbol_env, symbol)?;
            for p in args.iter_mut() {
                resolve_pat(phox, module, symbol_env, p)?;
            }
            Ok(())
        }
        Pat::Tuple(ps) => {
            for p in ps.iter_mut() {
                resolve_pat(phox, module, symbol_env, p)?;
            }
            Ok(())
        }
        Pat::Record(fields) => {
            for (_, p) in fields.iter_mut() {
                resolve_pat(phox, module, symbol_env, p)?;
            }
            Ok(())
        }
        _ => Ok(())
    }
}

// -------------------------------------------------------------
// === symbol ===
pub fn make_top_level_symbol(
    phox: &mut PhoxEngine,
    module: &RefModule,
    name: &str,
) -> Result<Symbol, TypeError> {
    let symbol_env = &mut phox.get_symbol_env(module);
    make_symbol(phox, module, symbol_env, name)
}

pub fn make_symbol(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    name: &str,
) -> Result<Symbol, TypeError> {
    let mut symbol = Symbol::unresolved(name);
    resolve_symbol(phox, module, symbol_env, &mut symbol)?;
    Ok(symbol)
}

pub fn resolve_symbol(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    symbol: &mut Symbol,
) -> Result<(), TypeError> {
    if let Symbol::Unresolved(path) = symbol {
        let path = path.clone();
        match path {
            Path::Relative(_) => {
                if let Some(s) = symbol_env.get(&path) {
                    *symbol = s.clone();
                }
                else {
                    // Is an alias contained in the path?
                    let tmp_absolute_path = module.borrow().resolve_alias(&path);
                    if let Some(alias_path) = tmp_absolute_path {
                        let mut sym = Symbol::Unresolved(alias_path);
                        resolve_symbol(phox, module, symbol_env, &mut sym)?;
                        *symbol = sym;
                    }
                    // Otherwise, module top-level symbol, trait member, or local symbol
                    else {
                        let name = path.to_string();
                        // Is a known trait member's name?
                        if module.borrow().trait_members.values().any(|v| v.contains(&name)) {
                            let sym = Symbol::local(&name);
                            *symbol = sym;
                        }
                        // Is local?
                        else if symbol_env.is_local() {
                            let sym = Symbol::local(&name);
                            symbol_env.insert(path.clone(), sym.clone());
                            *symbol = sym;
                        }
                        // module top-level symbol
                        else {
                            let sym = Symbol::Local(module.borrow().path().concat_path(&path).pretty());
                            symbol_env.insert(path.clone(), sym.clone());
                            *symbol = sym;
                        }
                    }
                }
            }
            Path::Absolute(ref _xs) => {
                if let Some(global_sym) = phox.get_extern_symbol_env(module).get(&path) {
                    *symbol = global_sym.clone();
                }
                else {
                    // and then register target into corresponding tables.
                    // - `mod` sub-module             -> module.submods \NOTE Symbols shall not be assigned to the module itself.
                    // - `type` type constructor      -> module.icx.kind_env
                    // - `trait` trait name           -> module.trait_members
                    //   - `trait` member type scheme -> module.icx.trait_member_env
                    //   - `impl` member type scheme  -> phox.impl_member_env \TODO move to module scope
                    //   - `impl` expr                -> phox.impl_env        \TODO move to module scope
                    // - top-level var type scheme    -> module.icx.type_env
                    //   - top-level var expr         -> module.env
                    match path.resolve(module, &phox.roots) {
                        Some((m, None)) => {
                            // NOTE: A path specifying `module` itself is permitted only in `mod`/`use` statements.
                            return Err(TypeError::Expeted {
                                expected: "value".to_string(),
                                actual: format!("module {}", m.borrow().path().pretty()),
                            });
                        }
                        Some((m, Some(rem))) if rem.len() == 1 => {
                            let target_sym = phox.get_symbol_env(&m).get(&rem).ok_or(TypeError::UnknownPath(path.clone()))?;
                            let extern_sym = target_sym.clone(); // <- \NOTE may be an "extern" symbol?

                            // is Trait name ?
                            if let Some(xs) = m.borrow().trait_members.get(&rem.to_string()) {
                                module.borrow_mut().trait_members.insert(rem.to_string(), xs.clone());
                                for member_name in xs.iter() {
                                    let member_sym = Symbol::local(member_name); // <- \NOTE This must NOT be a unique symbol
                                    if let Some(member_schemes) = phox.get_infer_ctx(&m).get_trait_member_schemes(&member_sym) {
                                        phox.get_infer_ctx(module).extend_trait_member_schemes(&member_sym, member_schemes);
                                    }
                                    // \TODO import `impl_member_env`
                                }

                                *symbol = extern_sym.clone();
                                phox.get_extern_symbol_env(module).insert(path, extern_sym);
                            }
                            // is Data constructor or Variable ?
                            else if let Some(ty_sch) = phox.get_infer_ctx(&m).get_type_scheme(&target_sym) {
                                phox.get_infer_ctx(module).put_type_scheme(extern_sym.clone(), ty_sch);
                                if let Some(val) = phox.get_value_env(&m).get(&target_sym) {
                                    phox.get_value_env(module).insert(extern_sym.clone(), val);
                                }

                                *symbol = extern_sym.clone();
                                phox.get_extern_symbol_env(module).insert(path, extern_sym);
                            }
                            // is Type constructor ?
                            else if let Some(k) = phox.get_infer_ctx(&m).get_kind(&target_sym) {
                                phox.get_infer_ctx(module).put_kind(extern_sym.clone(), k.clone());

                                *symbol = extern_sym.clone();
                                phox.get_extern_symbol_env(module).insert(path, extern_sym);
                            }
                            else {
                                return Err(TypeError::UnknownPath(path.clone()));
                            }
                        }
                        _ => {
                            return Err(TypeError::UnknownPath(path.clone()));
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

// -------------------------------------------------------------
// === expr ===
pub fn resolve_expr(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    expr: &mut Expr,
) -> Result<(), TypeError> {
    match &mut expr.body {
        ExprBody::App(f, x) => {
            resolve_expr(phox, module, symbol_env, f)?;
            resolve_expr(phox, module, symbol_env, x)
        }
        ExprBody::Abs(pat, e) => {
            let mut symbol_env2 = symbol_env.duplicate();
            resolve_pat(phox, module, &mut symbol_env2, pat)?;
            resolve_expr(phox, module, &mut symbol_env2, e)
        }
        ExprBody::If(cond, e1, e2) => {
            resolve_expr(phox, module, symbol_env, cond)?;
            resolve_expr(phox, module, symbol_env, e1)?;
            resolve_expr(phox, module, symbol_env, e2)
        }
        ExprBody::Match(strut, arms) => {
            resolve_expr(phox, module, symbol_env, strut)?;
            for (pat, e) in arms {
                let mut symbol_env2 = symbol_env.duplicate();
                resolve_pat(phox, module, &mut symbol_env2, pat)?;
                resolve_expr(phox, module, &mut symbol_env2, e)?;
            }
            Ok(())
        }
        ExprBody::Tuple(es) => {
            for e in es {
                resolve_expr(phox, module, symbol_env, e)?;
            }
            Ok(())
        }
        ExprBody::Record(fields) => {
            for (_field, e) in fields {
                resolve_expr(phox, module, symbol_env, e)?;
            }
            Ok(())
        }
        ExprBody::FieldAccess(e, _field) => {
            resolve_expr(phox, module, symbol_env, e)
        }
        ExprBody::TupleAccess(e, _index) => {
            resolve_expr(phox, module, symbol_env, e)
        }
        ExprBody::Block(items) => {
            let mut symbol_env2 = symbol_env.duplicate();
            for item in items {
                resolve_item(phox, module, &mut symbol_env2, item)?;
            }
            Ok(())
        }
        ExprBody::RawTraitRecord(raw) => {
            let trait_head = resolve_raw_trait_head(phox, module, symbol_env, &raw, &HashMap::new())?;
            let base_score = trait_head.score();
            let mut matches = Vec::new();
            for (impl_sch, member_map) in phox.impl_env.iter() {
                // impl_sch: TraitScheme
                let (_impl_constraints, impl_head) = impl_sch.instantiate(&mut phox.ctx);

                // impl_head と required trait_head を unify
                if impl_head.name == trait_head.name && impl_head.score() == base_score {
                    let mut dummy_ctx = phox.ctx.clone();
                    if trait_head.unify(&mut dummy_ctx, &impl_head).is_ok() {
                        matches.push((impl_sch, member_map));
                    }
                }
            }
            match matches.len() {
                0 => {
                    // 実装が見つからない
                    Err(TypeError::MissingTraitImpl(trait_head.clone()))
                }
                1 => {
                    let (impl_sch, impls) = matches[0];
                    let (_impl_constraints, impl_head) = impl_sch.instantiate(&mut phox.ctx);
                    trait_head.unify(&mut phox.ctx, &impl_head)?;
                    let fields: Vec<(String, Expr)> = impls
                        .iter().map(|(k, v)| (k.to_string(), v.clone())).collect();
                    expr.body = ExprBody::Record(fields);
                    resolve_expr(phox, module, symbol_env, expr)
                }
                _ => {
                    let cand_traits: Vec<String> =
                        matches.into_iter().map(|(trait_sch, _)| trait_sch.target.to_string()).collect();
                    Err(TypeError::AmbiguousTrait {
                        trait_head: trait_head.to_string(),
                        candidates: cand_traits,
                    })
                }
            }
        }
        ExprBody::Var(ref mut symbol) => {
            resolve_symbol(phox, module, symbol_env, symbol)
        }
        ExprBody::Lit(_) => Ok(()),
    }
}

// -------------------------------------------------------------
// === type decl ===
use crate::interpreter::*;

fn register_type_decl(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    raw: &RawTypeDecl,
) -> Result<(), TypeError> {
    let TypeDecl::SumType {
        name: ty_ctor_name,
        params: ty_ctor_params,
        variants,
    } = &resolve_raw_type_decl(phox, module, symbol_env, raw)?;

    let icx = &mut phox.get_infer_ctx(module);
    let env = &mut phox.get_value_env(module);

    // kind を構築
    let mut kind = Kind::Star;
    for _ in ty_ctor_params.iter().rev() {
        kind = Kind::Fun(Box::new(Kind::Star), Box::new(kind));
    }
    icx.put_kind(ty_ctor_name.clone(), kind);

    // 各コンストラクタを登録
    for v in variants {
        // Type scheme of the data constructor `v`.
        icx.put_type_scheme(
            v.name(),
            v.as_scheme(ty_ctor_name, ty_ctor_params)
        );
        // Value of the data constructor `v`.
        env.insert(
            v.name(),
            make_constructor(&v.name(), v.arity())
        );
    }

    Ok(())
}

fn resolve_raw_type_decl(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    raw: &RawTypeDecl,
) -> Result<TypeDecl, TypeError> {
    match raw {
        RawTypeDecl::SumType { name, params, variants } => {
            let mut param_map = HashMap::new();
            let mut param_ids = Vec::new();
            for p in params {
                let id = phox.ctx.fresh_type_var_id();
                param_map.insert(p.clone(), id);
                param_ids.push(id);
            }

            let mut resolved_variants = Vec::new();
            for v in variants.into_iter() {
                let rv = resolve_raw_variant(phox, module, symbol_env, v, &param_map)?;
                resolved_variants.push(rv);
            }
            let symbol = make_symbol(phox, module, symbol_env, &name)?;
            Ok(TypeDecl::SumType {
                name: symbol,
                params: param_ids,
                variants: resolved_variants,
            })
        }
    }
}

/// RawVariant を解決して Variant に変換する
fn resolve_raw_variant(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    raw: &RawVariant,
    param_map: &HashMap<String, TypeVarId>,
) -> Result<Variant, TypeError> {
    let v = match raw {
        RawVariant::Unit(name) => {
            let symbol = make_symbol(phox, module, symbol_env, &name)?;
            Variant::Unit(symbol)
        }
        RawVariant::Tuple(name, elems) => {
            let symbol = make_symbol(phox, module, symbol_env, &name)?;
            let mut elems2 = Vec::new();
            for t in elems.iter() {
                let ty = resolve_raw_type(phox, module, symbol_env, t, &mut param_map.clone())?;
                elems2.push(ty);
            }
            Variant::Tuple(symbol, elems2)
        }
    };
    Ok(v)
}

/// RawType を解決して Type に変換する
fn resolve_raw_type(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    raw: &RawType,
    param_map: &mut HashMap<String, TypeVarId>,
) -> Result<Type, TypeError> {
    let ty = match raw {
        RawType::VarName(name) => {
            let id = param_map
                .entry(name.to_string())
                .or_insert_with(|| phox.ctx.fresh_type_var_id());
            Type::Var(*id)
        }
        RawType::ConName(symbol) => {
            let mut symbol = symbol.clone();
            resolve_symbol(phox, module, symbol_env, &mut symbol)?;
            Type::Con(symbol)
        }
        RawType::App(f, x) => {
            let f2 = resolve_raw_type(phox, module, symbol_env, f, param_map)?;
            let x2 = resolve_raw_type(phox, module, symbol_env, x, param_map)?;
            Type::App(Box::new(f2), Box::new(x2))
        }
        RawType::Fun(l, r) => {
            let l2 = resolve_raw_type(phox, module, symbol_env, l, param_map)?;
            let r2 = resolve_raw_type(phox, module, symbol_env, r, param_map)?;
            Type::Fun(Box::new(l2), Box::new(r2))
        }
        RawType::Tuple(elems) => {
            let mut elems2 = Vec::new();
            for t in elems.iter() {
                let ty = resolve_raw_type(phox, module, symbol_env, t, param_map)?;
                elems2.push(ty);
            }
            Type::Tuple(elems2)
        }
        RawType::Record(fields) => {
            let mut fields2 = Vec::new();
            for (fname, t) in fields.iter() {
                let ty = resolve_raw_type(phox, module, symbol_env, t, param_map)?;
                fields2.push((fname.clone(), ty));
            }
            Type::Record(fields2)
        }
    };
    Ok(ty)
}

// -------------------------------------------------------------
fn resolve_raw_trait_head(
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
    Ok(TraitHead {
        name: symbol,
        params,
    })
}
