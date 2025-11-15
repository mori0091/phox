use crate::api::PhoxEngine;
use crate::syntax::ast::*;
use crate::module::*;
use super::*;

// ===== Inference (Algorithm J core) =====
pub fn infer_item(phox: &mut PhoxEngine, icx: &mut InferCtx, item: &mut Item) -> Result<Type, TypeError> {
    match item {
        Item::Stmt(stmt) => {
            infer_stmt(phox, icx, stmt)
        }
        Item::Expr(expr) => {
            infer_expr(phox, icx, expr)
        }
        _ => Ok(Type::unit())
    }
}

pub fn infer_stmt(phox: &mut PhoxEngine, icx: &mut InferCtx, stmt: &mut Stmt) -> Result<Type, TypeError> {
    match stmt {
        Stmt::Mod(_) => Ok(Type::unit()),
        Stmt::Use(_) => Ok(Type::unit()),
        Stmt::Let(pat, expr) => {
            let t_expr = infer_expr(phox, icx, expr)?;
            let t_pat = phox.ctx.fresh_type_for_pattern(pat);
            phox.ctx.unify(&t_expr, &t_pat)?;
            let ref_icx = icx.clone(); // snapshot for reference
            phox.ctx.match_pattern(icx, pat, &t_pat, &ref_icx, true)?;
            Ok(Type::unit())
        }
        Stmt::LetRec(pat, expr) => {
            match pat {
                Pat::Var(x) => {
                    let tv = Type::Var(phox.ctx.fresh_type_var_id());
                    let mut icx2 = icx.clone();
                    {
                        icx2.type_env.insert(x.clone(), TypeScheme::mono(tv.clone()));
                        let t_expr = infer_expr(phox, &mut icx2, expr)?;
                        phox.ctx.unify(&tv, &t_expr)?;
                    }
                    let sch = generalize(&mut phox.ctx, icx, &tv);
                    icx.type_env.insert(x.clone(), sch);
                    Ok(Type::unit())
                }
                _ => Err(TypeError::LetRecPatternNotSupported(pat.clone())),
            }
        }
    }
}

pub fn infer_expr(phox: &mut PhoxEngine, icx: &mut InferCtx, expr: &mut Expr) -> Result<Type, TypeError> {
    let ty = match &mut expr.body {
        ExprBody::Var(symbol) => {
            match icx.type_env.get(symbol) {
                Some(sch) => {
                    let (_constraints, ty) = sch.instantiate(&mut phox.ctx);
                    ty
                }
                None => {
                    match phox.impl_member_env.get(symbol).clone() {
                        None => return Err(TypeError::UnboundVariable(symbol.clone())),
                        Some(cands) => {
                            let symbol = symbol.clone();
                            let cands: Vec<_> = cands.iter().cloned().collect();
                            Type::Overloaded(symbol, cands)
                        }
                    }
                }
            }
        }

        ExprBody::App(f, a) => {
            let ta = infer_expr(phox, icx, a)?;
            if let Type::Overloaded(name, candidates) = ta {
                let mut ctx = phox.ctx.clone();
                let candidates: Vec<_> = candidates
                    .into_iter()
                    .map(|tmpl| tmpl.fresh_copy(&mut ctx))
                    .collect();
                return Err(TypeError::AmbiguousVariable { name, candidates })
            }

            let tf = infer_expr(phox, icx, f)?;
            let tr = Type::Var(phox.ctx.fresh_type_var_id());

            match tf {
                Type::Overloaded(name, cands) => {
                    let mut filtered = Vec::new();
                    for cand_tmpl in cands {
                        // 1) ctx を複製して試行用の try_ctx を作る
                        let mut try_ctx = phox.ctx.clone();

                        // 2) 候補は try_ctx で fresh 化
                        let cand = cand_tmpl.fresh_copy(&mut try_ctx);
                        let (_, ty_inst) = &cand.instantiate(&mut try_ctx); // 制約の型引数のみ instantiate

                        // 3) フィルタ判定は「引数のみ」
                        if let Type::Fun(param, _) = &ty_inst {
                            if try_ctx.unify(&ta, param).is_ok() {
                                // eprintln!("try_ta vs param: {} vs {}", try_ta.repr(&mut try_ctx), param.repr(&mut try_ctx));
                                filtered.push((cand.target.score(), cand_tmpl));
                            }
                        }
                    }

                    if filtered.is_empty() {
                        return Err(TypeError::NoMatchingOverload);
                    }

                    let (best_score, winner_tmpl) = filtered.iter().max_by_key(|(score, _)| score).unwrap();
                    {
                        let candidates: Vec<_> = filtered.iter().filter(|(score, _)| score == best_score).cloned().collect();
                        if candidates.len() > 1 {
                            let mut ctx = phox.ctx.clone();
                            let candidates: Vec<_> = candidates
                                .into_iter()
                                .map(|(_, tmpl)| tmpl.fresh_copy(&mut ctx))
                                .collect();
                            return Err(TypeError::AmbiguousVariable { name, candidates })
                        }
                    }

                    let winner = winner_tmpl.fresh_copy(&mut phox.ctx);
                    let (_, ty_inst) = winner.instantiate(&mut phox.ctx);
                    if let Type::Fun(param, ret) = ty_inst.clone() {
                        // eprintln!("ta vs param: {} vs {}", ta.repr(ctx), param.repr(ctx));
                        let ta = ta.repr(&mut phox.ctx);
                        phox.ctx.unify(&ta, &param)?;
                        phox.ctx.unify(&tr, &ret)?;
                    }
                    f.ty = Some(ty_inst.repr(&mut phox.ctx));
                    // App(f, a) の型
                    tr
                }

                other => {
                    phox.ctx.unify(&other, &Type::fun(ta, tr.clone()))?;
                    tr
                }
            }
        }

        ExprBody::Abs(pat, body) => {
            // パターンに対応する型を生成
            let t_pat = phox.ctx.fresh_type_for_pattern(&pat);

            // 環境を拡張
            let mut icx2 = icx.clone();
            // ctx.match_pattern(&mut env2, pat, &t_pat, tenv)?;
            phox.ctx.match_pattern(&mut icx2, &pat, &t_pat, icx, false)?;

            // 本体を推論
            let t_body = infer_expr(phox, &mut icx2, body)?;

            // 関数型を返す
            Type::fun(t_pat, t_body)
        }

        ExprBody::Block(items) => {
            let mut icx2 = icx.clone(); // 新しいスコープ
            let mut last_ty = Type::unit();
            for item in items.iter_mut() {
                last_ty = infer_item(phox, &mut icx2, item)?;
            }
            last_ty
        }
        ExprBody::If(cond, then_e, else_e) => {
            let t_cond = infer_expr(phox, icx, cond)?;
            phox.ctx.unify(&t_cond, &Type::bool_())?;

            let t_then = infer_expr(phox, icx, then_e)?;
            let t_else = infer_expr(phox, icx, else_e)?;
            phox.ctx.unify(&t_then, &t_else)?;
            t_then
        }

        ExprBody::Match(scrutinee, arms) => {
            // 判別対象式の型を推論
            let t_scrut = infer_expr(phox, icx, scrutinee)?;

            // 各アームの式型を集める
            let mut result_types = vec![];

            for (pat, body) in arms.iter_mut() {
                // パターンに対応する型を生成（型変数を含む構造）
                let t_pat = phox.ctx.fresh_type_for_pattern(&pat);

                // scrutinee の型とパターン型を unify
                phox.ctx.unify(&t_scrut, &t_pat)?;

                // 束縛環境を構築
                let mut env2 = icx.clone();
                phox.ctx.match_pattern(&mut env2, &pat, &t_pat, icx, false)?;

                // アーム本体の型を推論
                let t_body = infer_expr(phox, &mut env2, body)?;
                result_types.push(t_body);
            }

            let mut result_types = result_types.into_iter();
            let t_result = result_types.next().ok_or(TypeError::EmptyMatch)?;
            for ty in result_types {
                phox.ctx.unify(&t_result, &ty)?;
            }

            t_result
        }

        ExprBody::Lit(Lit::Unit) => Type::unit(),
        ExprBody::Lit(Lit::Bool(_)) => Type::bool_(),
        ExprBody::Lit(Lit::Int(_)) => Type::int(),

        ExprBody::Tuple(es) => {
            let mut tys = Vec::with_capacity(es.len());
            for e in es.iter_mut() {
                let ty = infer_expr(phox, icx, e)?;
                tys.push(ty);
            }
            Type::Tuple(tys)
        }

        ExprBody::Record(fields) => {
            // 各フィールドの型を推論
            let mut typed_fields = Vec::with_capacity(fields.len());
            for (fname, fexpr) in fields.iter_mut() {
                let t_field = infer_expr(phox, icx, fexpr)?;
                typed_fields.push((fname.clone(), t_field));
            }
            Type::Record(typed_fields)
        }

        ExprBody::FieldAccess(base, field) => {
            let t_base = infer_expr(phox, icx, base)?;
            match t_base {
                Type::Record(fields) => {
                    if let Some((_, ty)) = fields.iter().find(|(fname, _)| fname == field) {
                        ty.clone()
                    } else {
                        return Err(TypeError::UnknownField(field.clone(), Type::Record(fields)));
                    }
                }
                other => {
                    if let Some(con) = is_tycon(&other) {
                        // let pat = Pat::Con(con, vec![Pat::local_var("r")]);
                        let pat = Pat::Con(con, vec![Pat::unresolved_var("r")]);
                        let p = base.clone();
                        let mut expr = Expr::block(vec![
                            Item::Stmt(Stmt::Let(pat, p)),
                            // Item::Expr(Expr::field_access(Expr::local_var("r"), field.clone()))
                            Item::Expr(Expr::field_access(Expr::unresolved_var("r"), field.clone()))
                        ]);
                        let ty = infer_expr(phox, icx, &mut expr).map_err(|_| TypeError::ExpectedRecord(other))?;
                        ty
                    }
                    else {
                        return Err(TypeError::ExpectedRecord(other));
                    }
                }
            }
        }

        ExprBody::TupleAccess(base, index) => {
            let t_base = infer_expr(phox, icx, base)?;
            match t_base {
                Type::Tuple(elems) => {
                    if *index < elems.len() {
                        elems[*index].clone()
                    } else {
                        return Err(TypeError::IndexOutOfBounds(*index, Type::Tuple(elems)));
                    }
                }
                other => {
                    if let Some(con) = is_tycon(&other) {
                        // let pat = Pat::Con(con, vec![Pat::local_var("t")]);
                        let pat = Pat::Con(con, vec![Pat::unresolved_var("t")]);
                        let p = base.clone();
                        let mut expr = Expr::block(vec![
                            Item::Stmt(Stmt::Let(pat, p)),
                            // Item::Expr(Expr::tuple_access(Expr::local_var("t"), *index))
                            Item::Expr(Expr::tuple_access(Expr::unresolved_var("t"), *index))
                        ]);
                        let ty = infer_expr(phox, icx, &mut expr).map_err(|_| TypeError::ExpectedTuple(other))?;
                        ty
                    }
                    else {
                        return Err(TypeError::ExpectedTuple(other));
                    }
                }
            }
        }

        ExprBody::RawTraitRecord(_) => {
            unreachable!()
        }
    };
    expr.ty = Some(ty.clone());
    Ok(ty)
}

fn is_tycon(mut t: &Type) -> Option<Symbol> {
    while let Type::App(a, _) = t {
        t = a;
    }
    match t {
        Type::Con(con) => Some(con.clone()),
        _ => None,
    }
}
