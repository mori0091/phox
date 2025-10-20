use std::collections::HashMap;
use crate::typesys::{TypeContext, TypeVarId, Type};
use super::{RawTypeDecl, RawVariant, RawType};
use super::{TypeDecl, Variant};
use super::{Item, Stmt, Expr};

pub fn resolve_item(
    ctx: &mut TypeContext,
    tenv: &mut TypeEnv,
    env: &mut Env,
    item: &Item,
) {
    match item {
        Item::RawTypeDecl(raw) => {
            let tydecl = resolve_raw_type_decl(ctx, raw.clone());
            register_type_decl(&tydecl, tenv, env);
        }
        Item::Stmt(stmt) => {
            resolve_stmt(ctx, tenv, env, stmt)
        }
        Item::Expr(expr) => {
            resolve_expr(ctx, tenv, env, expr)
        }
    }
}

pub fn resolve_stmt(
    ctx: &mut TypeContext,
    tenv: &mut TypeEnv,
    env: &mut Env,
    stmt: &Stmt,
) {
    match stmt {
        Stmt::Let(_p, expr) | Stmt::LetRec(_p, expr) => {
            resolve_expr(ctx, tenv, env, expr)
        }
    }
}

pub fn resolve_expr(
    ctx: &mut TypeContext,
    tenv: &mut TypeEnv,
    env: &mut Env,
    expr: &Expr,
) {
    match expr {
        Expr::App(f, x) => {
            resolve_expr(ctx, tenv, env, f);
            resolve_expr(ctx, tenv, env, x);
        }
        Expr::Abs(_p, e) => {
            resolve_expr(ctx, tenv, env, e);
        }
        Expr::If(cond, e1, e2) => {
            resolve_expr(ctx, tenv, env, cond);
            resolve_expr(ctx, tenv, env, e1);
            resolve_expr(ctx, tenv, env, e2);
        }
        Expr::Match(strut, arms) => {
            resolve_expr(ctx, tenv, env, strut);
            for (_p, e) in arms {
                resolve_expr(ctx, tenv, env, e);
            }
        }
        Expr::Tuple(es) => {
            for e in es {
                resolve_expr(ctx, tenv, env, e);
            }
        }
        Expr::Record(fields) => {
            for (_field, e) in fields {
                resolve_expr(ctx, tenv, env, e);
            }
        }
        Expr::FieldAccess(e, _field) => {
            resolve_expr(ctx, tenv, env, e);
        }
        Expr::TupleAccess(e, _index) => {
            resolve_expr(ctx, tenv, env, e);
        }
        Expr::Block(items) => {
            for item in items {
                resolve_item(ctx, tenv, env, item);
            }
        }
        Expr::Lit(_) | Expr::Var(_) => (),
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
                .map(|t| resolve_raw_type(ctx, t, param_map))
                .collect();
            Variant::Tuple(name, elems2)
        }
    }
}

/// RawType を解決して Type に変換する
pub fn resolve_raw_type(
    ctx: &mut TypeContext,
    raw: RawType,
    param_map: &HashMap<String, TypeVarId>,
) -> Type {
    match raw {
        RawType::VarName(name) => {
            if let Some(&id) = param_map.get(&name) {
                Type::Var(id)
            } else {
                // 未定義の型変数はエラー
                panic!("Unbound type variable in data constructor: {}", name);
                // // GADT等、多相的なデータ構築子を許すなら、未知の型変数名は
                // // fresh で新規に割り当てる
                // let id = ctx.fresh_type_var_id();
                // Type::Var(id)
            }
        }
        RawType::ConName(name) => Type::Con(name),
        RawType::App(f, x) => {
            let f2 = resolve_raw_type(ctx, *f, param_map);
            let x2 = resolve_raw_type(ctx, *x, param_map);
            Type::App(Box::new(f2), Box::new(x2))
        }
        RawType::Fun(l, r) => {
            let l2 = resolve_raw_type(ctx, *l, param_map);
            let r2 = resolve_raw_type(ctx, *r, param_map);
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
                .map(|(fname, ty)| (fname, resolve_raw_type(ctx, ty, param_map)))
                .collect();
            Type::Record(fields2)
        }
    }
}

// use crate::typesys::{Kind, KindEnv, TypeEnv};
use crate::typesys::TypeEnv;
use crate::interpreter::Env;
use crate::interpreter::make_constructor;

pub fn register_type_decl(
    decl: &TypeDecl,
    // kind_env: &mut KindEnv,
    type_env: &mut TypeEnv,
    env: &mut Env,
) {
    // register_type(decl, kind_env, type_env);
    register_type(decl, type_env);
    register_variants(decl, env);
}

// pub fn register_type(decl: &TypeDecl, kenv: &mut KindEnv, tenv: &mut TypeEnv
// ) {
//     match decl {
//         TypeDecl::SumType { name, params, variants } => {
//             // kind を構築
//             let mut kind = Kind::Star;
//             for _ in params.iter().rev() {
//                 kind = Kind::Arrow(Box::new(Kind::Star), Box::new(kind));
//             }
//             kenv.insert(name.clone(), kind);
//             // 各コンストラクタを登録
//             for v in variants {
//                 let (ctor_name, ctor_scheme) = v.as_scheme(name, params);
//                 tenv.insert(ctor_name.clone(), ctor_scheme.clone());
//             }
//         }
//     }
// }
pub fn register_type(decl: &TypeDecl, tenv: &mut TypeEnv) {
    match decl {
        TypeDecl::SumType { name, params, variants } => {
            // 各コンストラクタを登録
            for v in variants {
                let (ctor_name, ctor_scheme) = v.as_scheme(name, params);
                tenv.insert(ctor_name.clone(), ctor_scheme.clone());
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
