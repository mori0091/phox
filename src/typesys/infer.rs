use crate::api::PhoxEngine;
use crate::syntax::ast::*;
use crate::module::*;
use super::*;

// ===== Inference (Algorithm J core) =====
pub fn infer_item(
    phox: &mut PhoxEngine,
    module: &RefModule,
    icx: &mut InferCtx,
    item: &mut Item
) -> Result<Type, TypeError> {
    match item {
        Item::Decl(decl) => {
            infer_decl(phox, module, icx, decl)
        }
        Item::Stmt(stmt) => {
            infer_stmt(phox, module, icx, stmt)
        }
        Item::Expr(expr) => {
            infer_expr(phox, module, icx, expr)
        }
    }
}

pub fn infer_decl(
    phox: &mut PhoxEngine,
    module: &RefModule,
    icx: &mut InferCtx,
    decl: &mut Decl,
) -> Result<Type, TypeError> {
    match decl {
        Decl::Type(_) | Decl::Trait(_) => Ok(Type::unit()),

        Decl::Impl(_) => unreachable!(),

        Decl::ImplResolved(Impl { head_sch, members }) => {
            check_impl_comflict(phox, &head_sch)?;
            for m in members.iter() {
                let icx = &mut icx.duplicate();
                let ty = infer_expr(phox, module, icx, &mut m.expr.clone())?;
                let sch = m.sch_tmpl.fresh_copy(&mut phox.ctx);
                let (_, ty_inst) = &sch.instantiate(&mut phox.ctx);
                phox.ctx.unify(&ty, ty_inst)?;
            }

            for m in members.iter() {
                phox.impl_env
                    .entry(head_sch.clone())
                    .or_default()
                    .insert(m.symbol.clone(), m.expr.clone());
                phox.impl_member_env
                    .entry(m.symbol.clone())
                    .or_default()
                    .insert(m.sch_tmpl.clone());
            }
            Ok(Type::unit())
        }
    }
}

fn check_impl_comflict(
    phox: &mut PhoxEngine,
    impl_head_sch: &Scheme<TraitHead>,
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

pub fn infer_stmt(
    phox: &mut PhoxEngine,
    module: &RefModule,
    icx: &mut InferCtx,
    stmt: &mut Stmt
) -> Result<Type, TypeError> {
    match stmt {
        Stmt::Use(_) => Ok(Type::unit()),
        Stmt::Mod(name, items) => {
            let sub = module.get_submod(name)
                .ok_or_else(|| {
                    let p = module.borrow().path().concat_str(&[name]);
                    TypeError::UnknownPath(p)
                })?;
            if let Some(items) = items {
                let icx2 = &mut phox.get_infer_ctx(&sub);
                for item in items.iter_mut() {
                    infer_item(phox, &sub, icx2, item)?;
                }
            }
            Ok(Type::unit())
        }
        Stmt::Let(pat, expr) => {
            let t_expr = infer_expr(phox, module, icx, expr)?;
            let t_pat = phox.ctx.fresh_type_for_pattern(pat);
            phox.ctx.unify(&t_expr, &t_pat)?;
            let ref_icx = icx.duplicate(); // snapshot for reference
            phox.ctx.match_pattern(icx, pat, &t_pat, &ref_icx, true)?;
            Ok(Type::unit())
        }
        Stmt::LetRec(pat, expr) => {
            match pat {
                Pat::Var(x) => {
                    let tv = Type::Var(phox.ctx.fresh_type_var_id());
                    let mut icx2 = icx.duplicate();
                    {
                        icx2.put_type_scheme(x.clone(), TypeScheme::mono(tv.clone()));
                        let t_expr = infer_expr(phox, module, &mut icx2, expr)?;
                        phox.ctx.unify(&tv, &t_expr)?;
                    }
                    let sch = generalize(&mut phox.ctx, icx, &tv);
                    icx.put_type_scheme(x.clone(), sch);
                    Ok(Type::unit())
                }
                _ => Err(TypeError::UnsupportedLetRecPattern(pat.clone())),
            }
        }
    }
}

pub fn infer_expr(
    phox: &mut PhoxEngine,
    module: &RefModule,
    icx: &mut InferCtx,
    expr: &mut Expr
) -> Result<Type, TypeError> {
    let ty = match &mut expr.body {
        ExprBody::Var(symbol) => {
            match icx.get_type_scheme(symbol) {
                Some(sch) => {
                    let (_constraints, ty) = sch.instantiate(&mut phox.ctx);
                    ty
                }
                None => {
                    match phox.impl_member_env.get(symbol).clone() {
                        None => return Err(TypeError::UnboundVariable(symbol.clone())),
                        Some(cands) if cands.len() == 1 => {
                            let tmpl = &cands.iter().cloned().next().unwrap();
                            let winner = tmpl.fresh_copy(&mut phox.ctx);
                            let (_, ty_inst) = winner.instantiate(&mut phox.ctx);
                            ty_inst
                        }
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
            let ta = infer_expr(phox, module, icx, a)?;
            if let Type::Overloaded(name, candidates) = ta {
                let mut ctx = phox.ctx.clone();
                let candidates: Vec<_> = candidates
                    .into_iter()
                    .map(|tmpl| tmpl.fresh_copy(&mut ctx))
                    .collect();
                return Err(TypeError::AmbiguousVariable { name, candidates })
            }

            let tf = infer_expr(phox, module, icx, f)?;
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
                        return Err(TypeError::NoMatchingOverload(*f.clone()));
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
            let mut icx2 = icx.duplicate();
            // ctx.match_pattern(&mut env2, pat, &t_pat, tenv)?;
            phox.ctx.match_pattern(&mut icx2, &pat, &t_pat, icx, false)?;

            // 本体を推論
            let t_body = infer_expr(phox, module, &mut icx2, body)?;

            // 関数型を返す
            Type::fun(t_pat, t_body)
        }

        ExprBody::Block(items) => {
            let mut icx2 = icx.duplicate(); // 新しいスコープ
            let mut last_ty = Type::unit();
            for item in items.iter_mut() {
                last_ty = infer_item(phox, module, &mut icx2, item)?;
            }
            last_ty
        }
        ExprBody::If(cond, then_e, else_e) => {
            let t_cond = infer_expr(phox, module, icx, cond)?;
            phox.ctx.unify(&t_cond, &Type::bool_())?;

            let t_then = infer_expr(phox, module, icx, then_e)?;
            let t_else = infer_expr(phox, module, icx, else_e)?;
            phox.ctx.unify(&t_then, &t_else)?;
            t_then
        }

        ExprBody::Match(scrutinee, arms) => {
            // 判別対象式の型を推論
            let t_scrut = infer_expr(phox, module, icx, scrutinee)?;

            // 各アームの式型を集める
            let mut result_types = vec![];

            for (pat, body) in arms.iter_mut() {
                // パターンに対応する型を生成（型変数を含む構造）
                let t_pat = phox.ctx.fresh_type_for_pattern(&pat);

                // scrutinee の型とパターン型を unify
                phox.ctx.unify(&t_scrut, &t_pat)?;

                // 束縛環境を構築
                let mut env2 = icx.duplicate();
                phox.ctx.match_pattern(&mut env2, &pat, &t_pat, icx, false)?;

                // アーム本体の型を推論
                let t_body = infer_expr(phox, module, &mut env2, body)?;
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
                let ty = infer_expr(phox, module, icx, e)?;
                tys.push(ty);
            }
            Type::Tuple(tys)
        }

        ExprBody::Record(fields) => {
            // 各フィールドの型を推論
            let mut typed_fields = Vec::with_capacity(fields.len());
            for (fname, fexpr) in fields.iter_mut() {
                let t_field = infer_expr(phox, module, icx, fexpr)?;
                typed_fields.push((fname.clone(), t_field));
            }
            Type::Record(typed_fields)
        }

        ExprBody::FieldAccess(base, field) => {
            let t_base = infer_expr(phox, module, icx, base)?;
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
                        let ty = infer_expr(phox, module, icx, &mut expr)
                            .map_err(|_| TypeError::ExpectedRecord(other))?;
                        ty
                    }
                    else {
                        return Err(TypeError::ExpectedRecord(other));
                    }
                }
            }
        }

        ExprBody::TupleAccess(base, index) => {
            let t_base = infer_expr(phox, module, icx, base)?;
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
                        let ty = infer_expr(phox, module, icx, &mut expr)
                            .map_err(|_| TypeError::ExpectedTuple(other))?;
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
