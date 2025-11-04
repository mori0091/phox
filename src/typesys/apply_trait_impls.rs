use crate::syntax::ast::*;
use super::*;
use crate::module::Module;
use std::cell::RefMut;

pub fn apply_trait_impls_item(
    item: &mut Item,
    ctx: &mut TypeContext,
    module: &mut RefMut<Module>,
) -> Result<(), TypeError> {
    match item {
        Item::RawTraitDecl(_) => Ok(()), // trait decl: no need to apply trait impls
        Item::RawImplDecl(_) => Ok(()),  // impl decl : no need to apply trait impls
        Item::RawTypeDecl(_) => Ok(()),  // type decl : no need to apply trait impls
        Item::Expr(expr) => apply_trait_impls_expr(expr, ctx, module),
        Item::Stmt(stmt) => apply_trait_impls_stmt(stmt, ctx, module),
    }
}

pub fn apply_trait_impls_stmt(
    stmt: &mut Stmt,
    ctx: &mut TypeContext,
    module: &mut RefMut<Module>,
) -> Result<(), TypeError> {
    match stmt {
        Stmt::Let(_pat, expr) => apply_trait_impls_expr(expr, ctx, module),
        Stmt::LetRec(_pat, expr) => apply_trait_impls_expr(expr, ctx, module),
    }
}

pub fn apply_trait_impls_expr(
    expr: &mut Expr,
    ctx: &mut TypeContext,
    module: &mut RefMut<Module>,
) -> Result<(), TypeError> {
    // 再帰的に子ノードを処理
    match &mut expr.body {
        ExprBody::App(f, a) => {
            apply_trait_impls_expr(f, ctx, module)?;
            apply_trait_impls_expr(a, ctx, module)?;
        }
        ExprBody::Abs(_, body) => {
            apply_trait_impls_expr(body, ctx, module)?;
        }
        ExprBody::If(cond, then_, else_) => {
            apply_trait_impls_expr(cond, ctx, module)?;
            apply_trait_impls_expr(then_, ctx, module)?;
            apply_trait_impls_expr(else_, ctx, module)?;
        }
        ExprBody::Match(scrutinee, arms) => {
            apply_trait_impls_expr(scrutinee, ctx, module)?;
            for (_, arm_expr) in arms.iter_mut() {
                apply_trait_impls_expr(arm_expr, ctx, module)?;
            }
        }
        ExprBody::Tuple(es) => {
            for e in es.iter_mut() {
                apply_trait_impls_expr(e, ctx, module)?;
            }
        }
        ExprBody::Record(fields) => {
            for (_, e) in fields.iter_mut() {
                apply_trait_impls_expr(e, ctx, module)?;
            }
        }
        ExprBody::TupleAccess(e, _) | ExprBody::FieldAccess(e, _) => {
            apply_trait_impls_expr(e, ctx, module)?;
        }
        ExprBody::Block(items) => {
            for item in items.iter_mut() {
                apply_trait_impls_item(item, ctx, module)?;
            }
        }
        ExprBody::Var(name) => {
            // 型情報が必要なので、型が推論済みであることを確認
            let ty = expr.ty.as_ref().ok_or(TypeError::MissingType)?;

            // 推論器で解決しきれなかったエラーをここで拾う
            if let Type::Overloaded(name, cands) = ty {
                return Err(TypeError::AmbiguousVariable {
                    name: name.clone(), // 元の変数名
                    candidates: cands.clone(),
                });
            }

            if module.icx.trait_member_env.contains_key(name) {
                // このメンバに必要な制約を構築（型から導出）
                let constraints = TraitHead::from_trait_member(ctx, &module.icx.trait_member_env, name, ty)?;

                let mut matches = Vec::new();
                for (impl_head, member_map) in module.impl_env.iter() {
                    // impl_sch: TraitScheme
                    let (_impl_constraints, impl_head) = impl_head.instantiate(ctx);

                    for constraint in constraints.iter() {
                        // impl_head と required constraint を unify
                        if impl_head.name == constraint.name {
                            let mut try_ctx = ctx.clone();
                            if constraint.unify(&mut try_ctx, &impl_head).is_ok() {
                                if let Some(impl_expr) = member_map.get(name) {
                                    matches.push((constraint.clone(), impl_expr.clone()));
                                }
                            }
                        }
                    }
                }
                match matches.len() {
                    0 => {
                        // 実装が見つからない → 何もしない
                        // 通常の変数参照なら infer_expr 側で処理される
                    }
                    _ => {
                        // let (_, impl_expr) = matches.pop().unwrap();
                        let (_, impl_expr) = matches.iter().min_by_key(|(c, _)| c.score()).unwrap();
                        expr.body = impl_expr.body.clone();
                        // expr.ty は既に推論済みなのでそのままでOK
                    }
                    // _ => {
                    //     let cand_traits: Vec<String> =
                    //         matches.iter().map(|(c, _)| c.to_string()).collect();
                    //     return Err(TypeError::AmbiguousTraitMember {
                    //         member: name.clone(),
                    //         ty: ty.clone(),
                    //         candidates: cand_traits,
                    //     });
                    // }
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
