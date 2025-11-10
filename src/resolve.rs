use std::cell::RefMut;
use std::collections::HashMap;

use crate::typesys::*;
use crate::syntax::ast::*;
use crate::module::*;
use crate::api::PhoxEngine;

pub fn resolve_item(
    phox: &mut PhoxEngine,
    module: &mut RefMut<Module>,
    symbol_env: &mut SymbolEnv,
    item: &mut Item,
) -> Result<(), TypeError> {
    match item {
        Item::RawTraitDecl(raw) => {
            register_trait(module, raw);
            Ok(())
        }
        Item::RawImplDecl(raw) => {
            register_impl(phox, module, symbol_env, raw)
        }
        Item::RawTypeDecl(raw) => {
            register_type_decl(phox, module, raw);
            Ok(())
        }
        Item::Stmt(stmt) => {
            resolve_stmt(phox, module, symbol_env, stmt)
        }
        Item::Expr(expr) => {
            resolve_expr(phox, module, symbol_env, expr)
        }
    }
}

pub fn register_trait(
    module: &mut RefMut<Module>,
    raw: &RawTraitDecl,
) {
    let head = RawTraitHead {
        name: raw.name.clone(),
        params: raw
            .params
            .iter()
            .map(|name| RawType::VarName(name.clone()))
            .collect(),
    };
    for member in raw.members.iter() {
        let sch = RawTypeScheme {
            vars: raw.params.clone(),
            constraints: vec![head.clone()],
            target: *member.ty.clone(),
        };
        module.icx.trait_member_env
            .entry(Symbol::Local(member.name.clone()))
            .or_default()
            .insert(sch);
    }
}

pub fn register_impl(
    phox: &mut PhoxEngine,
    module: &mut RefMut<Module>,
    symbol_env: &mut SymbolEnv,
    raw: &RawImplDecl,
) -> Result<(), TypeError> {
    let impl_head = RawTraitHead {
        name: raw.name.clone(),
        params: raw.params.clone(),
    };

    for member in raw.members.iter() {
        let trait_schemes = module.icx
            .trait_member_env
            .get(&Symbol::Local(member.name.clone()))
            .ok_or(TypeError::UnknownTraitMember(member.name.clone()))?;
        if trait_schemes.is_empty() {
            return Err(TypeError::UnknownTraitMember(member.name.clone()));
        }

        let trait_scheme = trait_schemes
            .iter()
            .find(|sch| sch.constraints[0].name == raw.name)
            .ok_or(TypeError::UnknownTrait(raw.name.clone()))?;

        let trait_head = &trait_scheme.constraints[0];
        if impl_head.params.len() != trait_head.params.len() {
            return Err(TypeError::ArityMismatch {
                trait_name: Symbol::Local(trait_head.name.clone()),
                member: member.name.clone(),
                expected: trait_head.params.len(),
                actual: impl_head.params.len()
            });
        }

        let impl_scheme = {
            let mut subst = HashMap::new();
            for (t1, t2) in trait_head.params.iter().zip(impl_head.params.iter()) {
                match t1 {
                    RawType::VarName(a) => {
                        subst.insert(a.clone(), t2.clone());
                    }
                    other => {
                        if other != t2 {
                            let empty_map = HashMap::new();
                            let expected = resolve_raw_type(&mut phox.ctx, other, &empty_map);
                            let actual   = resolve_raw_type(&mut phox.ctx, t2, &empty_map);
                            return Err(TypeError::Mismatch(expected, actual));
                        }
                    }
                }
            }
            // eprintln!("subst: {:?}", subst);
            trait_scheme.apply_subst(&subst).generalize()
        };

        // eprintln!("trait: @{{{}}}.{}: {}", trait_head, member.name, trait_scheme.pretty());
        // eprintln!("impl : @{{{}}}.{}: {}", impl_head, member.name, impl_scheme.pretty());

        phox.impl_member_env
            .entry(Symbol::Local(member.name.clone()))
            .or_default()
            .insert(impl_scheme);
    };

    let trait_sch = {
        let trait_head = {
            let name = Symbol::Local(raw.name.clone());
            let params: Vec<Type> = raw
                .params
                .iter()
                .map(|raw_ty| resolve_raw_type(&mut phox.ctx, raw_ty, &HashMap::new()))
                .collect();
            TraitHead { name, params }
        };
        generalize(&mut phox.ctx, &module.icx, &trait_head)
    };

    let mut member_map = HashMap::new();
    for member in raw.members.iter() {
        let mut expr = member.expr.clone();
        resolve_expr(phox, module, symbol_env, &mut expr)?;
        member_map.insert(Symbol::Local(member.name.clone()), *expr);
    }

    // {
    //     eprintln!("impl {} {{", trait_sch);
    //     for (name, expr) in member_map.iter() {
    //         eprintln!("  {} = {}", name, expr);
    //     }
    //     eprintln!("}}");
    // }
    phox.impl_env.insert(trait_sch, member_map);

    Ok(())
}

pub fn resolve_stmt(
    phox: &mut PhoxEngine,
    module: &mut RefMut<Module>,
    symbol_env: &mut SymbolEnv,
    stmt: &mut Stmt,
) -> Result<(), TypeError> {
    match stmt {
        Stmt::Mod(_m) => todo!(),
        Stmt::Use(_p) => todo!(),
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

pub fn resolve_pat(
    phox: &mut PhoxEngine,
    module: &mut RefMut<Module>,
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

pub fn resolve_symbol(
    phox: &mut PhoxEngine,
    module: &mut RefMut<Module>,
    symbol_env: &mut SymbolEnv,
    symbol: &mut Symbol,
) -> Result<(), TypeError> {
    if let Symbol::Unresolved(path) = symbol {
        match path {
            Path::Absolute(_) => {
                if let Some(s) = phox.global_symbol_env.get(path) {
                    *symbol = s.clone();
                } else {
                    let s = Symbol::Extern(GlobalId::new());
                    phox.global_symbol_env.insert(path.clone(), s.clone());
                    *symbol = s;

                    // and then register target into corresponding tables.
                    // - `impl` expr                  -> phox.impl_env
                    // - `impl` member type scheme    -> phox.impl_member_env
                    // - `trait` member type scheme   -> module.icx.trait_member_env
                    // - `type` type constructor      -> module.icx.kind_env
                    // - top-level `expr` type scheme -> module.icx.type_env
                    // - top-level `expr` value       -> module.env
                }
            }
            Path::Relative(_) => {
                if let Some(s) = symbol_env.get(path) {
                    *symbol = s.clone();
                } else if let Some(alias_path) = module.resolve_alias(path) {
                    resolve_symbol(phox, module, symbol_env, &mut Symbol::Unresolved(alias_path))?;
                } else {
                    let local = Symbol::Local(path.to_string());
                    symbol_env.insert(path.clone(), local.clone());
                    *symbol = local;
                }
            }
        }
    }
    Ok(())
}

pub fn resolve_expr(
    phox: &mut PhoxEngine,
    module: &mut RefMut<Module>,
    symbol_env: &mut SymbolEnv,
    expr: &mut Expr,
) -> Result<(), TypeError> {
    match &mut expr.body {
        ExprBody::App(f, x) => {
            resolve_expr(phox, module, symbol_env, f)?;
            resolve_expr(phox, module, symbol_env, x)
        }
        ExprBody::Abs(pat, e) => {
            let mut symbol_env2 = symbol_env.clone();
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
                let mut symbol_env2 = symbol_env.clone();
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
            let mut symbol_env2 = symbol_env.clone();
            for item in items {
                resolve_item(phox, module, &mut symbol_env2, item)?;
            }
            Ok(())
        }
        ExprBody::RawTraitRecord(raw) => {
            let trait_head = resolve_raw_trait_head(&mut phox.ctx, &raw, &HashMap::new());
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
                    Ok(())
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

pub fn resolve_raw_type_decl(
    phox: &mut PhoxEngine,
    raw: &RawTypeDecl,
) -> TypeDecl {
    match raw {
        RawTypeDecl::SumType { name, params, variants } => {
            let mut param_map = HashMap::new();
            let mut param_ids = Vec::new();
            for p in params {
                let id = phox.ctx.fresh_type_var_id();
                param_map.insert(p.clone(), id);
                param_ids.push(id);
            }

            let resolved_variants = variants
                .into_iter()
                .map(|v| resolve_raw_variant(phox, v, &param_map))
                .collect();

            TypeDecl::SumType {
                name: Symbol::Local(name.to_string()),
                params: param_ids,
                variants: resolved_variants,
            }
        }
    }
}

/// RawVariant を解決して Variant に変換する
pub fn resolve_raw_variant(
    phox: &mut PhoxEngine,
    raw: &RawVariant,
    param_map: &HashMap<String, TypeVarId>,
) -> Variant {
    match raw {
        RawVariant::Unit(name) => Variant::Unit(Symbol::Local(name.to_string())),
        RawVariant::Tuple(name, elems) => {
            let elems2 = elems
                .into_iter()
                .map(|t| resolve_raw_type(&mut phox.ctx, &t, param_map))
                .collect();
            Variant::Tuple(Symbol::Local(name.to_string()), elems2)
        }
    }
}

/// RawType を解決して Type に変換する
pub fn resolve_raw_type(
    ctx: &mut TypeContext,
    raw: &RawType,
    param_map: &HashMap<String, TypeVarId>,
) -> Type {
    match raw {
        RawType::VarName(name) => {
            if let Some(&id) = param_map.get(name) {
                Type::Var(id)
            } else {
                // GADT等、多相的なデータ構築子を許すなら、未知の型変数名は
                // fresh で新規に割り当てる
                let id = ctx.fresh_type_var_id();
                Type::Var(id)
            }
        }
        RawType::ConName(name) => Type::local_con(name),
        RawType::App(f, x) => {
            let f2 = resolve_raw_type(ctx, f, param_map);
            let x2 = resolve_raw_type(ctx, x, param_map);
            Type::App(Box::new(f2), Box::new(x2))
        }
        RawType::Fun(l, r) => {
            let l2 = resolve_raw_type(ctx, l, param_map);
            let r2 = resolve_raw_type(ctx, r, param_map);
            Type::Fun(Box::new(l2), Box::new(r2))
        }
        RawType::Tuple(elems) => {
            let elems2 = elems
                .into_iter()
                .map(|t| resolve_raw_type(ctx, t, param_map))
                .collect();
            Type::Tuple(elems2)
        }
        RawType::Record(fields) => {
            let fields2 = fields
                .into_iter()
                .map(|(fname, ty)| (fname.clone(), resolve_raw_type(ctx, ty, param_map)))
                .collect();
            Type::Record(fields2)
        }
    }
}

use crate::interpreter::Env;
use crate::interpreter::make_constructor;

pub fn register_type_decl(
    phox: &mut PhoxEngine,
    module: &mut RefMut<Module>,
    raw: &RawTypeDecl,
) {
    let decl = resolve_raw_type_decl(phox, raw);
    register_type(&decl, &mut module.icx);
    register_variants(&decl, &mut module.env);
}

pub fn register_type(decl: &TypeDecl, icx: &mut InferCtx) {
    match decl {
        TypeDecl::SumType { name, params, variants } => {
            // kind を構築
            let mut kind = Kind::Star;
            for _ in params.iter().rev() {
                kind = Kind::Fun(Box::new(Kind::Star), Box::new(kind));
            }
            icx.kind_env.insert(name.clone(), kind);
            // 各コンストラクタを登録
            for v in variants {
                let (ctor_name, ctor_scheme) = v.as_scheme(name, params);
                icx.type_env.insert(Symbol::Local(ctor_name.clone()), ctor_scheme.clone());
            }
        }
    }
}

pub fn register_variants(decl: &TypeDecl, env: &mut Env) {
    match decl {
        TypeDecl::SumType { name:_, params:_, variants } => {
            // 各コンストラクタを登録
            for v in variants {
                let arity = match v {
                    Variant::Unit(_) => 0,
                    Variant::Tuple(_, ts) => ts.len(),
                };
                env.insert(v.name(),
                           make_constructor(&v.name(), arity));
            }
        }
    }
}

pub fn resolve_raw_trait_head(
    ctx: &mut TypeContext,
    raw: &RawTraitHead,
    param_map: &HashMap<String, TypeVarId>,
) -> TraitHead {
    let params = raw
        .params
        .iter()
        .map(|p| resolve_raw_type(ctx, p, param_map))
        .collect();

    TraitHead {
        name: Symbol::Local(raw.name.clone()),
        params,
    }
}
