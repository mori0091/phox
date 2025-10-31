use std::collections::HashMap;
use crate::typesys::{generalize, ImplEnv, InferCtx, RawTypeScheme, TypeContext};
use crate::typesys::{TypeVarId, Kind, Type, Constraint};
use crate::typesys::TypeError;
use crate::syntax::ast::{RawTraitDecl, RawImplDecl};
use crate::syntax::ast::{RawTypeDecl, RawVariant, RawType};
use crate::syntax::ast::RawConstraint;
use crate::syntax::ast::{TypeDecl, Variant};
use crate::syntax::ast::{Item, Stmt, Expr, ExprBody};

pub fn resolve_item(
    ctx: &mut TypeContext,
    icx: &mut InferCtx,
    impl_env: &mut ImplEnv,
    env: &mut Env,
    item: &mut Item,
) -> Result<(), TypeError> {
    match item {
        Item::RawTraitDecl(raw) => {
            register_trait(ctx, icx, raw);
            Ok(())
        }
        Item::RawImplDecl(raw) => {
            register_impl(ctx, icx, impl_env, env, raw)
        }
        Item::RawTypeDecl(raw) => {
            let tydecl = resolve_raw_type_decl(ctx, raw.clone());
            register_type_decl(&tydecl, icx, env);
            Ok(())
        }
        Item::Stmt(stmt) => {
            resolve_stmt(ctx, icx, impl_env, env, stmt)
        }
        Item::Expr(expr) => {
            resolve_expr(ctx, icx, impl_env, env, expr)
        }
    }
}

pub fn register_trait(
    _ctx: &mut TypeContext,
    icx: &mut InferCtx,
    raw: &RawTraitDecl,
) {
    let head = RawConstraint {
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
        icx.trait_member_env
            .entry(member.name.clone())
            .or_default()
            .insert(sch);
    }
}

pub fn register_impl(
    ctx: &mut TypeContext,
    icx: &mut InferCtx,
    impl_env: &mut ImplEnv,
    env: &mut Env,
    raw: &RawImplDecl,
) -> Result<(), TypeError> {
    let impl_head = RawConstraint {
        name: raw.name.clone(),
        params: raw.params.clone(),
    };

    for member in raw.members.iter() {
        let trait_schemes = icx
            .trait_member_env
            .get(&member.name)
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
                trait_name: trait_head.name.clone(),
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
                            let expected = resolve_raw_type(ctx, other, &empty_map);
                            let actual   = resolve_raw_type(ctx, t2, &empty_map);
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

        icx.impl_member_env
            .entry(member.name.clone())
            .or_default()
            .insert(impl_scheme);
    };

    let trait_sch = {
        let constraint = {
            let name = raw.name.clone();
            let params: Vec<Type> = raw
                .params
                .iter()
                .map(|raw_ty| resolve_raw_type(ctx, raw_ty, &HashMap::new()))
                .collect();
            Constraint { name, params }
        };
        generalize(ctx, icx, &constraint)
    };

    let mut member_map = HashMap::new();
    for member in raw.members.iter() {
        let mut expr = member.expr.clone();
        resolve_expr(ctx, icx, impl_env, env, &mut expr)?;
        member_map.insert(member.name.clone(), *expr);
    }

    // {
    //     eprintln!("impl {} {{", trait_sch);
    //     for (name, expr) in member_map.iter() {
    //         eprintln!("  {} = {}", name, expr);
    //     }
    //     eprintln!("}}");
    // }
    impl_env.insert(trait_sch, member_map);

    Ok(())
}

pub fn resolve_stmt(
    ctx: &mut TypeContext,
    icx: &mut InferCtx,
    impl_env: &mut ImplEnv,
    env: &mut Env,
    stmt: &mut Stmt,
) -> Result<(), TypeError> {
    match stmt {
        Stmt::Let(_p, expr) | Stmt::LetRec(_p, expr) => {
            resolve_expr(ctx, icx, impl_env, env, expr)
        }
    }
}

pub fn resolve_expr(
    ctx: &mut TypeContext,
    icx: &mut InferCtx,
    impl_env: &mut ImplEnv,
    env: &mut Env,
    expr: &mut Expr,
) -> Result<(), TypeError> {
    match &mut expr.body {
        ExprBody::App(f, x) => {
            resolve_expr(ctx, icx, impl_env, env, f)?;
            resolve_expr(ctx, icx, impl_env, env, x)
        }
        ExprBody::Abs(_p, e) => {
            resolve_expr(ctx, icx, impl_env, env, e)
        }
        ExprBody::If(cond, e1, e2) => {
            resolve_expr(ctx, icx, impl_env, env, cond)?;
            resolve_expr(ctx, icx, impl_env, env, e1)?;
            resolve_expr(ctx, icx, impl_env, env, e2)
        }
        ExprBody::Match(strut, arms) => {
            resolve_expr(ctx, icx, impl_env, env, strut)?;
            for (_p, e) in arms {
                resolve_expr(ctx, icx, impl_env, env, e)?;
            }
            Ok(())
        }
        ExprBody::Tuple(es) => {
            for e in es {
                resolve_expr(ctx, icx, impl_env, env, e)?;
            }
            Ok(())
        }
        ExprBody::Record(fields) => {
            for (_field, e) in fields {
                resolve_expr(ctx, icx, impl_env, env, e)?;
            }
            Ok(())
        }
        ExprBody::FieldAccess(e, _field) => {
            resolve_expr(ctx, icx, impl_env, env, e)
        }
        ExprBody::TupleAccess(e, _index) => {
            resolve_expr(ctx, icx, impl_env, env, e)
        }
        ExprBody::Block(items) => {
            for item in items {
                resolve_item(ctx, icx, impl_env, env, item)?;
            }
            Ok(())
        }
        ExprBody::RawTraitRecord(raw) => {
            let constraint = resolve_raw_constraint(ctx, &raw, &HashMap::new());
            let base_score = constraint.score();
            let mut matches = Vec::new();
            for (impl_sch, member_map) in impl_env.iter() {
                // impl_sch: TraitScheme
                let (_impl_constraints, impl_head) = impl_sch.instantiate(ctx);

                // impl_head と required constraint を unify
                if impl_head.name == constraint.name && impl_head.score() == base_score {
                    let mut dummy_ctx = ctx.clone();
                    if constraint.unify(&mut dummy_ctx, &impl_head).is_ok() {
                        matches.push((impl_sch, member_map));
                    }
                }
            }
            match matches.len() {
                0 => {
                    // 実装が見つからない
                    Err(TypeError::MissingTraitImpl(constraint.clone()))
                }
                1 => {
                    let (impl_sch, impls) = matches[0];
                    let (_impl_constraints, impl_head) = impl_sch.instantiate(ctx);
                    constraint.unify(ctx, &impl_head)?;
                    let fields: Vec<(String, Expr)> = impls
                        .iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                    expr.body = ExprBody::Record(fields);
                    Ok(())
                }
                _ => {
                    let cand_traits: Vec<String> =
                        matches.into_iter().map(|(trait_sch, _)| trait_sch.target.to_string()).collect();
                    Err(TypeError::AmbiguousTrait {
                        constraint: constraint.to_string(),
                        candidates: cand_traits,
                    })
                }
            }
        }
        ExprBody::Lit(_) | ExprBody::Var(_) => Ok(()),
    }
}

pub fn resolve_raw_type_decl(
    ctx: &mut TypeContext,
    raw: RawTypeDecl,
) -> TypeDecl {
    match raw {
        RawTypeDecl::SumType { name, params, variants } => {
            let mut param_map = HashMap::new();
            let mut param_ids = Vec::new();
            for p in params {
                let id = ctx.fresh_type_var_id();
                param_map.insert(p.clone(), id);
                param_ids.push(id);
            }

            let resolved_variants = variants
                .into_iter()
                .map(|v| resolve_raw_variant(ctx, v, &param_map))
                .collect();

            TypeDecl::SumType {
                name,
                params: param_ids,
                variants: resolved_variants,
            }
        }
    }
}

/// RawVariant を解決して Variant に変換する
pub fn resolve_raw_variant(
    ctx: &mut TypeContext,
    raw: RawVariant,
    param_map: &HashMap<String, TypeVarId>,
) -> Variant {
    match raw {
        RawVariant::Unit(name) => Variant::Unit(name),
        RawVariant::Tuple(name, elems) => {
            let elems2 = elems
                .into_iter()
                .map(|t| resolve_raw_type(ctx, &t, param_map))
                .collect();
            Variant::Tuple(name, elems2)
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
        RawType::ConName(name) => Type::con(name),
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
    decl: &TypeDecl,
    icx: &mut InferCtx,
    env: &mut Env,
) {
    register_type(decl, icx);
    register_variants(decl, env);
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
                icx.type_env.insert(ctor_name.clone(), ctor_scheme.clone());
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
                    // Variant::Record(_, _) => 1,
                };
                env.insert(v.name(),
                           make_constructor(&v.name(), arity));
            }
        }
    }
}

pub fn resolve_raw_constraint(
    ctx: &mut TypeContext,
    raw: &RawConstraint,
    param_map: &HashMap<String, TypeVarId>,
) -> Constraint {
    let params = raw
        .params
        .iter()
        .map(|p| resolve_raw_type(ctx, p, param_map))
        .collect();

    Constraint {
        name: raw.name.clone(),
        params,
    }
}
