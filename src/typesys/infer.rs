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
) -> Result<(Type, Vec<Constraint>), Error> {
    match item {
        Item::Decl(_decl) => {
            Ok((Type::unit(), vec![]))
        }
        Item::Stmt(stmt) => {
            infer_stmt(phox, module, icx, stmt)
        }
        Item::Expr(expr) => {
            infer_expr(phox, module, icx, expr)
        }
    }
}

pub fn infer_stmt(
    phox: &mut PhoxEngine,
    module: &RefModule,
    icx: &mut InferCtx,
    stmt: &mut Stmt
) -> Result<(Type, Vec<Constraint>), Error> {
    match stmt {
        Stmt::Use(_) | Stmt::Mod(_, _) => Ok((Type::unit(), vec![])),
        Stmt::Let(pat, expr) => {
            let mut cs = Vec::new();
            let (t_expr, c_expr) = infer_expr(phox, module, icx, expr)?;
            let t_pat = phox.ctx.fresh_type_for_pattern(pat);
            cs.extend(c_expr);
            cs.push(Constraint::type_eq(&t_expr, &t_pat));
            // ★ need to solve here. (since `match_pattern` requires)
            solve(phox, cs)?;

            let ref_icx = icx.duplicate(); // snapshot for reference
            phox.ctx.match_pattern(icx, pat, &t_pat, &ref_icx, true)?;

            Ok((Type::unit(), vec![]))
        }
        Stmt::LetRec(pat, expr) => {
            match pat {
                Pat::Var(x) => {
                    let tv = Type::Var(phox.ctx.fresh_type_var_id());
                    let mut icx2 = icx.duplicate();
                    icx2.put_type_scheme(x.clone(), TypeScheme::mono(tv.clone()));

                    let mut cs = Vec::new();
                    let (t_expr, c_expr) = infer_expr(phox, module, &mut icx2, expr)?;
                    cs.extend(c_expr);
                    cs.push(Constraint::type_eq(&tv, &t_expr));
                    // ★ need to solve here. (since `generalize` requires)
                    solve(phox, cs)?;

                    let sch = generalize(&mut phox.ctx, icx, &tv);
                    icx.put_type_scheme(x.clone(), sch);

                    Ok((Type::unit(), vec![]))
                }
                _ => Err(Error::UnsupportedLetRecPattern(pat.clone())),
            }
        }
    }
}

pub fn infer_expr(
    phox: &mut PhoxEngine,
    module: &RefModule,
    icx: &mut InferCtx,
    expr: &mut Expr
) -> Result<(Type, Vec<Constraint>), Error> {
    if let Some(ty) = expr.ty.clone() {
        return Ok((ty, vec![]))
    }
    let (ty, cs) = match &mut expr.body {
        ExprBody::Var(symbol) => {
            match icx.get_type_scheme(symbol) {
                Some(sch) => {
                    let (constraints, ty) = sch.instantiate(&mut phox.ctx);
                    (ty, constraints.into_vec())
                }
                None => {
                    match phox.impl_member_env.get(symbol).clone() {
                        None => return Err(Error::UnboundVariable(symbol.clone())),
                        Some(cands) if cands.len() == 1 => {
                            let tmpl = &cands.iter().cloned().next().unwrap();
                            let winner = tmpl.fresh_copy(&mut phox.ctx);
                            let (constraints, ty_inst) = winner.instantiate(&mut phox.ctx);
                            (ty_inst, constraints.into_vec())
                        }
                        Some(cands) => {
                            let symbol = symbol.clone();
                            let cands: Vec<_> = cands.iter().cloned().collect();
                            let ty = Type::Var(phox.ctx.fresh_type_var_id());
                            (ty.clone(), vec![Constraint::Overloaded(symbol, ty, cands)])
                        }
                    }
                }
            }
        }

        ExprBody::App(f, a) => {
            let mut cs = Vec::new();
            let (ta, ca) = infer_expr(phox, module, icx, a)?;
            let (tf, cf) = infer_expr(phox, module, icx, f)?;
            let tr = Type::Var(phox.ctx.fresh_type_var_id());
            cs.extend(ca);
            cs.extend(cf);
            cs.push(Constraint::type_eq(&tf, &Type::fun(ta, tr.clone())));
            (tr, cs)
        }

        ExprBody::Abs(pat, body) => {
            // パターンに対応する型を生成
            let t_pat = phox.ctx.fresh_type_for_pattern(&pat);

            // 環境を拡張
            let mut icx2 = icx.duplicate();
            phox.ctx.match_pattern(&mut icx2, &pat, &t_pat, icx, false)?;

            // 本体を推論
            let (t_body, cs) = infer_expr(phox, module, &mut icx2, body)?;

            // 関数型を返す
            (Type::fun(t_pat, t_body), cs)
        }

        ExprBody::Block(items) => {
            let mut icx2 = icx.duplicate(); // 新しいスコープ
            let mut last_ty = Type::unit();
            let mut cs = Vec::new();
            for item in items.iter_mut() {
                let (ty, last_cs) = infer_item(phox, module, &mut icx2, item)?;
                cs.extend(last_cs);
                last_ty = ty;
            }
            (last_ty, cs)
        }
        ExprBody::If(cond, then_e, else_e) => {
            let mut cs = Vec::new();
            let (t_cond, cs_cond) = infer_expr(phox, module, icx, cond)?;
            let (t_then, cs_then) = infer_expr(phox, module, icx, then_e)?;
            let (t_else, cs_else) = infer_expr(phox, module, icx, else_e)?;
            cs.extend(cs_cond);
            cs.push(Constraint::type_eq(&t_cond, &Type::bool_()));
            cs.extend(cs_then);
            cs.extend(cs_else);
            cs.push(Constraint::type_eq(&t_then, &t_else));
            (t_then, cs)
        }

        ExprBody::Match(scrutinee, arms) => {
            // 判別対象式の型を推論
            let (t_scrut, c_scrut) = infer_expr(phox, module, icx, scrutinee)?;
            solve(phox, c_scrut)?;

            // 各アームの式型を集める
            let mut result_types = vec![];
            let mut cs = Vec::new();
            for (pat, body) in arms.iter_mut() {
                // パターンに対応する型を生成（型変数を含む構造）
                let t_pat = phox.ctx.fresh_type_for_pattern(&pat);

                // scrutinee の型とパターン型を unify
                // ★ need to solve here.
                // Why: Same as `let` pattern binding needs the concrete shape.
                solve(phox, vec![Constraint::type_eq(&t_scrut, &t_pat)])?;

                // 束縛環境を構築
                let mut env2 = icx.duplicate();
                phox.ctx.match_pattern(&mut env2, &pat, &t_pat, icx, false)?;

                // アーム本体の型を推論
                let (t_body, c_body) = infer_expr(phox, module, &mut env2, body)?;
                result_types.push(t_body);
                cs.extend(c_body);
            }

            let mut result_types = result_types.into_iter();
            let t_result = result_types.next().ok_or(Error::EmptyMatch)?;
            for ty in result_types {
                cs.push(Constraint::type_eq(&t_result, &ty));
            }

            (t_result, cs)
        }

        ExprBody::Lit(Lit::Unit) => (Type::unit(), vec![]),
        ExprBody::Lit(Lit::Bool(_)) => (Type::bool_(), vec![]),
        ExprBody::Lit(Lit::Int(_)) => (Type::int(), vec![]),

        ExprBody::Tuple(es) => {
            let mut tys = Vec::with_capacity(es.len());
            let mut css = Vec::new();
            for e in es.iter_mut() {
                let (ty, cs) = infer_expr(phox, module, icx, e)?;
                tys.push(ty);
                css.extend(cs);
            }
            (Type::Tuple(tys), css)
        }

        ExprBody::Record(fields) => {
            // 各フィールドの型を推論
            let mut typed_fields = Vec::with_capacity(fields.len());
            let mut css = Vec::new();
            for (fname, fexpr) in fields.iter_mut() {
                let (t_field, cs) = infer_expr(phox, module, icx, fexpr)?;
                typed_fields.push((fname.clone(), t_field));
                css.extend(cs);
            }
            (Type::Record(typed_fields), css)
        }

        ExprBody::FieldAccess(base, field) => {
            let mut css = Vec::new();
            let (t_base, c_base) = infer_expr(phox, module, icx, base)?;
            css.extend(c_base);
            match t_base {
                Type::Record(fields) => {
                    if let Some((_, ty)) = fields.iter().find(|(fname, _)| fname == field) {
                        (ty.clone(), css)
                    } else {
                        return Err(Error::UnknownField(field.clone(), Type::Record(fields)));
                    }
                }
                other => {
                    if let Some(con) = is_tycon(&other) {
                        let pat = Pat::Con(con, vec![Pat::unresolved_var("r")]);
                        let p = base.clone();
                        let mut expr = Expr::block(vec![
                            Item::Stmt(Stmt::Let(pat, p)),
                            Item::Expr(Expr::field_access(Expr::unresolved_var("r"), field.clone()))
                        ]);
                        let (ty, cs) = infer_expr(phox, module, icx, &mut expr)
                            .map_err(|_| Error::ExpectedRecord(other))?;
                        css.extend(cs);
                        (ty, css)
                    }
                    else {
                        return Err(Error::ExpectedRecord(other));
                    }
                }
            }
        }

        ExprBody::TupleAccess(base, index) => {
            let mut css = Vec::new();

            let (t_base, c_base) = infer_expr(phox, module, icx, base)?;
            css.extend(c_base);

            match t_base {
                Type::Tuple(elems) => {
                    if *index < elems.len() {
                        (elems[*index].clone(), css)
                    } else {
                        return Err(Error::IndexOutOfBounds(*index, Type::Tuple(elems)));
                    }
                }
                other => {
                    if let Some(con) = is_tycon(&other) {
                        let pat = Pat::Con(con, vec![Pat::unresolved_var("t")]);
                        let p = base.clone();
                        let mut expr = Expr::block(vec![
                            Item::Stmt(Stmt::Let(pat, p)),
                            Item::Expr(Expr::tuple_access(Expr::unresolved_var("t"), *index))
                        ]);
                        let (ty, cs) = infer_expr(phox, module, icx, &mut expr)
                            .map_err(|_| Error::ExpectedTuple(other))?;
                        css.extend(cs);
                        (ty, css)
                    }
                    else {
                        return Err(Error::ExpectedTuple(other));
                    }
                }
            }
        }

        ExprBody::RawTraitRecord(_) => {
            unreachable!()
        }
    };
    expr.ty = Some(ty.clone());
    Ok((ty, cs))
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
