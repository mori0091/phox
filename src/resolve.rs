use std::collections::HashMap;
use crate::typesys::{generalize, ImplEnv, InferCtx, TypeContext};
use crate::typesys::{TypeVarId, Kind, Type, Constraint, TypeScheme, ApplySubst};
use crate::typesys::TypeError;
use crate::typesys::infer_expr;
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
    ctx: &mut TypeContext,
    icx: &mut InferCtx,
    raw: &RawTraitDecl,
) {
    // 1. 型変数名を TypeVarId に変換
    let mut var_ids = Vec::new();
    let mut var_map = HashMap::new();
    for name in &raw.params {
        let id = ctx.fresh_type_var_id();
        var_map.insert(name.clone(), id);
        var_ids.push(id);
    }

    // 2. 各メンバを TypeScheme に変換して登録
    for member in &raw.members {
        let raw_ty = &member.ty;
        let ty = resolve_raw_type(ctx, raw_ty, &var_map);

        let constraint = Constraint {
            name: raw.name.clone(),
            params: var_ids.iter().map(|v| Type::Var(*v)).collect(),
        };

        let scheme = TypeScheme::new(var_ids.clone(), vec![constraint], ty);

        // trait メンバは member_env に登録する。type_env には登録しない。
        // - 曖昧さ早期発見のため; infer_expr で必要
        //   -> infer_expr で曖昧さが残る場合は即エラー
        // - 曖昧さ解消のため; apply_trait_impls_expr で必要(だった)
        //   -> 現在の仕様では apply_trait_impls_expr の時点では曖昧さは残ってないはず
        icx.trait_member_env
           .entry(member.name.clone())
           .or_default()
           .insert(scheme);
    }
}

use std::collections::HashSet;
use crate::typesys::FreeTypeVars;

pub fn register_impl(
    ctx: &mut TypeContext,
    icx: &mut InferCtx,
    impl_env: &mut ImplEnv,
    env: &mut Env,
    raw: &RawImplDecl,
) -> Result<(), TypeError> {
    // 1. 型引数を RawType → Type に変換
    let params: Vec<Type> = raw.params.iter()
        .map(|raw_ty| resolve_raw_type(ctx, raw_ty, &HashMap::new()))
        .collect();

    // 2. 制約キーを構築
    let constraint = Constraint { name: raw.name.clone(), params: params.clone() };

    // 3. メンバ辞書を構築しながら型検査
    let mut member_map = HashMap::new();
    for member in &raw.members {
        // 3a. trait_member_env から該当 trait のメンバスキームを取得（clone で借用を切る）
        let scheme = {
            let schemes = icx.trait_member_env
                .get(&member.name)
                .ok_or_else(|| TypeError::UnknownTraitMember(member.name.clone()))?;

            schemes.iter()
                .find(|sch| sch.constraints.iter().any(|c| c.name == raw.name))
                .cloned()
                .ok_or_else(|| TypeError::UnknownTraitMember(member.name.clone()))?
        };

        // 3b. trait の量化変数と impl の具体型を対応付ける置換を作る
        // 例: vars = [a], params = [Int] → { a ↦ Int }
        if scheme.vars.len() != params.len() {
            return Err(TypeError::ArityMismatch {
                trait_name: raw.name.clone(),
                member: member.name.clone(),
                expected: scheme.vars.len(),
                actual: params.len(),
            });
        }
        let subst: HashMap<TypeVarId, Type> =
            scheme.vars.iter().cloned().zip(params.clone()).collect();

        // 3c. スキームを具象化（instantiate は使わず、元スキームへ apply）
        let concrete_sch = scheme.apply_subst(&subst);
        let expected_ty = concrete_sch.target.clone();
        let _expected_constraints = concrete_sch.constraints.clone();

        // 3d. impl の式を推論
        let mut expr = member.expr.clone();
        resolve_expr(ctx, icx, impl_env, env, &mut expr)?;
        let actual_ty = infer_expr(ctx, icx, &mut expr)?;

        // 3e. unify で型一致を確認（必要なら制約処理もここで）
        // eprintln!("member: {}, expected: {} , actual: {}", member.name, expected_ty.repr(ctx), actual_ty.repr(ctx));
        ctx.unify(&expected_ty, &actual_ty)?;

        // 3f. impl_member_env に具象化済みスキームを登録
        // ★ そのまま入れず、置換後のスキームを完全量化してから登録する
        let mut free = HashSet::new();
        // target 側
        expected_ty.free_type_vars(ctx, &mut free);
        // constraints 側のパラメータに現れる自由変数も加える
        for c in &_expected_constraints {
            for p in &c.params {
                p.free_type_vars(ctx, &mut free);
            }
        }

        // 量化変数一覧を整列
        let mut vars: Vec<TypeVarId> = free.into_iter().collect();
        vars.sort_by_key(|v| v.0);

        // 完全量化した新スキームを作る（repr は不要。推論中は生の型でOK）
        let generalized_concrete = TypeScheme::new(vars, _expected_constraints.clone(), expected_ty.clone());

        // 登録
        icx.impl_member_env
           .entry(member.name.clone())
           .or_default()
           .insert(generalized_concrete);

        // 3g. ImplEnv にも式の本体を登録
        member_map.insert(member.name.clone(), *expr);
    }

    // 4. ImplEnv に登録
    let trait_sch = generalize(ctx, icx, &constraint);
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
            let constraint = resolve_raw_constraint(ctx, &raw);
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
                _ => {
                    // (特殊化度, 汎用度) のスコアでソート
                    let mut sorted = matches;
                    sorted.sort_by_key(|(sch, _)| sch.target.score());

                    // 先頭が最も具体的
                    let best = &sorted[0];
                    let best_count = best.0.vars.len();

                    // 同じスコアの候補が複数あれば曖昧
                    if sorted.iter().take_while(|(sch,_)| sch.vars.len() == best_count).count() > 1 {
                        let cand_traits: Vec<String> =
                            sorted.into_iter().map(|(trait_sch, _)| trait_sch.target.to_string()).collect();
                        return Err(TypeError::AmbiguousTrait {
                            constraint: constraint.to_string(),
                            candidates: cand_traits,
                        });
                    }

                    let (impl_sch, impls) = best;
                    let (_impl_constraints, impl_head) = impl_sch.instantiate(ctx);
                    constraint.unify(ctx, &impl_head)?;
                    let fields: Vec<(String, Expr)> = impls
                        .iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                    expr.body = ExprBody::Record(fields);
                    Ok(())
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
                // // 未定義の型変数はエラー
                // panic!("Unbound type variable in data constructor: {}", name);
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
) -> Constraint {
    let params = raw
        .params
        .iter()
        .map(|p| resolve_raw_type(ctx, p, &HashMap::new()))
        .collect();

    Constraint {
        name: raw.name.clone(),
        params,
    }
}
