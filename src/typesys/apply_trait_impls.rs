use crate::syntax::ast::{Item, Stmt, Expr, ExprBody};
use super::{ImplEnv, InferCtx, TypeContext, TypeError, Constraint};

pub fn apply_trait_impls_item(
    item: &mut Item,
    ctx: &mut TypeContext,
    icx: &InferCtx,
    impl_env: &ImplEnv,
) -> Result<(), TypeError> {
    match item {
        Item::RawTraitDecl(_) => Ok(()), // trait decl: no need to apply trait impls
        Item::RawImplDecl(_) => Ok(()),  // impl decl : no need to apply trait impls
        Item::RawTypeDecl(_) => Ok(()),  // type decl : no need to apply trait impls
        Item::Expr(expr) => apply_trait_impls_expr(expr, ctx, icx, impl_env),
        Item::Stmt(stmt) => apply_trait_impls_stmt(stmt, ctx, icx, impl_env),
    }
}

pub fn apply_trait_impls_stmt(
    stmt: &mut Stmt,
    ctx: &mut TypeContext,
    icx: &InferCtx,
    impl_env: &ImplEnv,
) -> Result<(), TypeError> {
    match stmt {
        Stmt::Let(_pat, expr) => apply_trait_impls_expr(expr, ctx, icx, impl_env),
        Stmt::LetRec(_pat, expr) => apply_trait_impls_expr(expr, ctx, icx, impl_env),
    }
}

pub fn apply_trait_impls_expr(
    expr: &mut Expr,
    ctx: &mut TypeContext,
    icx: &InferCtx,
    impl_env: &ImplEnv,
) -> Result<(), TypeError> {
    // 再帰的に子ノードを処理
    match &mut expr.body {
        ExprBody::App(f, a) => {
            apply_trait_impls_expr(f, ctx, icx, impl_env)?;
            apply_trait_impls_expr(a, ctx, icx, impl_env)?;
        }
        ExprBody::Abs(_, body) => {
            apply_trait_impls_expr(body, ctx, icx, impl_env)?;
        }
        ExprBody::If(cond, then_, else_) => {
            apply_trait_impls_expr(cond, ctx, icx, impl_env)?;
            apply_trait_impls_expr(then_, ctx, icx, impl_env)?;
            apply_trait_impls_expr(else_, ctx, icx, impl_env)?;
        }
        ExprBody::Match(scrutinee, arms) => {
            apply_trait_impls_expr(scrutinee, ctx, icx, impl_env)?;
            for (_, arm_expr) in arms.iter_mut() {
                apply_trait_impls_expr(arm_expr, ctx, icx, impl_env)?;
            }
        }
        ExprBody::Tuple(es) => {
            for e in es.iter_mut() {
                apply_trait_impls_expr(e, ctx, icx, impl_env)?;
            }
        }
        ExprBody::Record(fields) => {
            for (_, e) in fields.iter_mut() {
                apply_trait_impls_expr(e, ctx, icx, impl_env)?;
            }
        }
        ExprBody::TupleAccess(e, _) | ExprBody::FieldAccess(e, _) => {
            apply_trait_impls_expr(e, ctx, icx, impl_env)?;
        }
        ExprBody::Block(items) => {
            for item in items.iter_mut() {
                apply_trait_impls_item(item, ctx, icx, impl_env)?;
            }
        }
        ExprBody::Var(name) => {
            // 型情報が必要なので、型が推論済みであることを確認
            let ty = expr.ty.as_ref().ok_or(TypeError::MissingType)?;

            // if icx.type_env.contains_key(name) {
            if icx.member_env.contains_key(name) {
                // このメンバに必要な制約を構築（型から導出）
                let constraints = Constraint::from_trait_member(ctx, &icx.member_env, name, ty)?;

                // 該当する実装候補を全部集める
                let mut matches: Vec<(&Constraint, &Expr)> = Vec::new();
                for constraint in constraints.iter() {
                    if let Some(impls) = impl_env.get(constraint) {
                        if let Some(impl_expr) = impls.get(name) {
                            matches.push((constraint, impl_expr));
                        }
                    }
                }
                match matches.len() {
                    0 => {
                        // 実装が見つからない → 何もしない
                        // 通常の変数参照なら infer_expr 側で処理される
                    }
                    1 => {
                        let (_, impl_expr) = matches.pop().unwrap();
                        expr.body = impl_expr.body.clone();
                        // expr.ty は既に推論済みなのでそのままでOK
                    }
                    _ => {
                        let cand_traits: Vec<String> =
                            matches.iter().map(|(c, _)| c.to_string()).collect();
                        return Err(TypeError::AmbiguousTraitMember {
                            member: name.clone(),
                            ty: ty.clone(),
                            candidates: cand_traits,
                        });
                    }
                }
            }
        }
        ExprBody::RawTraitRecord(_) => {
            unreachable!()
        }
        ExprBody::Lit(_) => {}
    }

    Ok(())
}
