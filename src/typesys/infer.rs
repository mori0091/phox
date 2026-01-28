use std::collections::BTreeSet;
use indexmap::IndexMap;
use indexmap::IndexSet;

use crate::api::PhoxEngine;
use crate::syntax::ast::*;
use crate::module::*;
use super::*;

fn filter_residual(
    phox: &mut PhoxEngine,
    residual: Vec<Constraint>,
    scheme_vars: IndexSet<Var>,
) -> (Vec<Constraint>, Vec<Constraint>) {
    let mut requires = Vec::new();
    let mut errors = Vec::new();

    for constraint in residual {
        match &constraint {
            Constraint::Ty(c) => {
                let c = c.repr(&mut phox.ctx.ty);
                match c {
                    TypeConstraint::TraitBound(ref head) => {
                        let fv = &mut IndexSet::new();
                        head.free_vars(&mut phox.ctx, fv);
                        if fv.iter().all(|v| scheme_vars.contains(v)) {
                            requires.push(Constraint::Ty(c));
                        } else {
                            errors.push(Constraint::Ty(c));
                        }
                    }
                    _ => errors.push(Constraint::Ty(c)),
                }
            }
        }
    }

    (requires, errors)
}

// ===== Inference (Algorithm J core) =====
pub fn infer_item(
    phox: &mut PhoxEngine,
    module: &RefModule,
    icx: &mut InferCtx,
    item: &mut Item
) -> Result<(Type, Vec<Constraint>), Error> {
    match &mut item.body {
        ItemBody::Decl(decl) => {
            infer_decl(phox, module, icx, decl)
        }
        ItemBody::Stmt(stmt) => {
            infer_stmt(phox, module, icx, stmt)
        }
        ItemBody::Expr(expr) => {
            infer_expr(phox, module, icx, expr)
        }
    }
}

pub fn infer_decl(
    phox: &mut PhoxEngine,
    module: &RefModule,
    icx: &mut InferCtx,
    decl: &mut Decl,
) -> Result<(Type, Vec<Constraint>), Error> {
    match decl {
        Decl::Type(_) | Decl::Trait(_) => {
            Ok((Type::unit(), vec![]))
        }

        Decl::RawImpl(_) => {
            unreachable!();
        }
        Decl::NamedImpl(named) => {
            let mut headvars = IndexSet::new();
            named.head.free_vars(&mut phox.ctx, &mut headvars);
            // ----------------------------------------------------------------------
            phox.ctx.set_non_touchable(&headvars);
            // ----------------------------------------------------------------------
            icx.enter_template_body();
            let res = infer_decl_named_impl(phox, module, icx, named);
            icx.leave_template_body();
            // ----------------------------------------------------------------------
            phox.ctx.clear_non_touchable();
            // ----------------------------------------------------------------------
            let scheme = res?;
            *decl = Decl::SchImpl(scheme);
            Ok((Type::unit(), vec![]))
        }
        Decl::SchImpl(_) => {
            Ok((Type::unit(), vec![]))
        }

        Decl::RawStarlet(_) => {
            unreachable!();
        }
        Decl::NamedStarlet(named) => {
            // ----------------------------------------------------------------------
            icx.enter_template_body();
            let res = infer_decl_named_starlet(phox, module, icx, named);
            icx.leave_template_body();
            // ----------------------------------------------------------------------
            let scheme = res?;
            *decl = Decl::SchStarlet(scheme);
            Ok((Type::unit(), vec![]))
        }
        Decl::SchStarlet(_) => {
            Ok((Type::unit(), vec![]))
        }
    }
}

fn infer_decl_named_impl(
    phox: &mut PhoxEngine,
    module: &RefModule,
    icx: &mut InferCtx,
    named: &NamedImpl,
) -> Result<Scheme<TypedImpl>, Error> {
    let trait_sch_tmpls = trait_members(phox, module, &named.head)
        .ok_or(Error::UnknownTrait(named.head.name.clone()))?;

    // infer type (try)
    for (sym, expr) in named.members.iter() {
        let sch_tmpl = trait_sch_tmpls
            .get(&sym.to_string())
            .ok_or(Error::UnknownTraitMember(sym.pretty()))?;
        let icx2 = &mut icx.duplicate();
        let (ty, _cs) = infer_expr(phox, module, icx2, &mut expr.clone())?;
        let mut dummy_ctx = phox.ctx.clone();
        let sch = sch_tmpl.fresh_copy(&mut dummy_ctx.ty);
        let (_, ty_inst) = &sch.instantiate(&mut dummy_ctx.ty);
        dummy_ctx.ty.unify(&ty, ty_inst)?;
    }

    // infer type
    let mut css = Vec::new();
    let mut members = Vec::new();
    for (sym, expr) in named.members.iter() {
        let sym = sym.clone();
        let mut expr = expr.repr(&mut phox.ctx.ty);

        let sch_tmpl = trait_sch_tmpls.get(&sym.to_string()).unwrap();
        let (ty, mut cs) = infer_expr(phox, module, icx, &mut expr)?;
        let sch = sch_tmpl.fresh_copy(&mut phox.ctx.ty);
        let (constraints, ty_inst) = &sch.instantiate(&mut phox.ctx.ty);
        cs.extend(constraints.into_vec_for_trait_record());
        cs.push(Constraint::type_eq(&ty, &ty_inst));
        if let Some(p) = &constraints.primary {
            let hd = *p.clone();
            for (t1, t2) in hd.params.iter().zip(named.head.params.iter()) {
                cs.push(Constraint::type_eq(t1, t2));
            }
        }
        css.extend(cs);

        members.push((sym, expr, ty));
    }

    let residual = solve_with_residual(phox, css)?;

    let typed = TypedImpl { head: named.head.clone(), members }.repr(&mut phox.ctx.ty);

    let mut scheme_vars = IndexSet::new();
    typed.free_vars(&mut phox.ctx, &mut scheme_vars);

    let (requires, errors) = filter_residual(phox, residual, scheme_vars.clone());

    solve_error(phox, errors)?;

    let scheme = Scheme::new(
        scheme_vars.into_iter().collect(),
        ConstraintSet {
            primary: None,
            requires: requires.into_iter().collect::<BTreeSet<_>>(),
        },
        typed,
    );

    Ok(scheme)
}

fn infer_decl_named_starlet(
    phox: &mut PhoxEngine,
    module: &RefModule,
    icx: &mut InferCtx,
    named: &NamedStarlet,
) -> Result<Scheme<TypedStarlet>, Error> {
    let name = named.name.clone();
    let mut expr = named.expr.clone();
    let ty = Type::Var(phox.ctx.ty.fresh_var_id());

    let mut css = Vec::new();
    let (t_expr, c_expr) = infer_expr(phox, module, icx, &mut expr)?;
    css.extend(c_expr);
    css.push(Constraint::type_eq(&ty, &t_expr));

    // ★ need to solve here. (since `generalize` requires)
    let residual = solve_with_residual(phox, css)?;

    let typed = TypedStarlet { name, expr, ty }.repr(&mut phox.ctx.ty);

    let mut scheme_vars = IndexSet::new();
    typed.free_vars(&mut phox.ctx, &mut scheme_vars);

    let (requires, errors) = filter_residual(phox, residual, scheme_vars.clone());

    solve_error(phox, errors)?;

    let scheme = Scheme::new(
        scheme_vars.into_iter().collect(),
        ConstraintSet {
            primary: None,
            requires: requires.into_iter().collect::<BTreeSet<_>>(),
        },
        typed,
    );

    Ok(scheme)
}

pub fn infer_stmt(
    phox: &mut PhoxEngine,
    module: &RefModule,
    icx: &mut InferCtx,
    stmt: &mut Stmt,
) -> Result<(Type, Vec<Constraint>), Error> {
    match stmt {
        Stmt::Use(_) => Ok((Type::unit(), vec![])),
        Stmt::Mod(_, _) => Ok((Type::unit(), vec![])),
        Stmt::Let(pat, expr) => {
            let mut css = Vec::new();
            let (t_expr, c_expr) = infer_expr(phox, module, icx, expr)?;
            let t_pat = phox.ctx.ty.fresh_type_for_pattern(pat);
            css.extend(c_expr);
            css.push(Constraint::type_eq(&t_expr, &t_pat));

            let monotype_bindings
                = icx.is_in_template_body()
                || (icx.is_local() && !expr.is_value());

            let mut bindings = Vec::new();
            let cs = phox.ctx.ty.match_pattern(icx, pat, &t_pat, monotype_bindings, &mut bindings)?;
            css.extend(cs);

            // assert!(bindings.is_empty()) if monotype_bindings;

            // ★ need to solve here. (since `generalize` requires)
            let pending = solve_with_residual(phox, css)?;

            for (sym, t) in bindings {
                let t = t.repr(&mut phox.ctx.ty);
                let sch = generalize(&mut phox.ctx, icx, &t);
                icx.put_type_scheme(sym, sch);
            }

            Ok((Type::unit(), pending))
        }
        Stmt::LetRec(pat, expr) => {
            match pat {
                Pat::Var(x) => {
                    let tv = Type::Var(phox.ctx.ty.fresh_var_id());
                    let mut icx2 = icx.duplicate();
                    icx2.put_type_scheme(x.clone(), TypeScheme::mono(tv.clone()));

                    let mut css = Vec::new();
                    let (t_expr, c_expr) = infer_expr(phox, module, &mut icx2, expr)?;
                    css.extend(c_expr);
                    css.push(Constraint::type_eq(&tv, &t_expr));

                    // ★ solve is required before repr (and before generalize in non-rec cases)
                    let pending = solve_with_residual(phox, css)?;
                    let tv = tv.repr(&mut phox.ctx.ty);

                    let sch = Scheme::mono(tv);
                    icx.put_type_scheme(x.clone(), sch);

                    Ok((Type::unit(), pending))
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
            if phox.starlet_env.is_starlet(&symbol) {
                let matches = phox.starlet_env.get_by_name(&symbol);
                match matches.len() {
                    0 => {
                        return Err(Error::NoMatchingOverload(expr.clone()));
                    }
                    1 => {
                        let tmpl = matches.first().unwrap();
                        let sch = tmpl.fresh_copy(&mut phox.ctx.ty);
                        let (constraints, typed_starlet) = sch.instantiate(&mut phox.ctx.ty);
                        (typed_starlet.ty.clone(), constraints.into_vec())
                    }
                    _ => {
                        let symbol = symbol.clone();
                        let mut cands = Vec::new();
                        for sch_tmpl in &matches {
                            let sch = sch_tmpl.fresh_copy(&mut phox.ctx.ty);
                            let member_sch = Scheme {
                                vars: sch.vars.clone(),
                                constraints: sch.constraints.clone(),
                                target: sch.target.ty.clone(),
                            };
                            let member_sch_tmpl = SchemeTemplate::new(member_sch);
                            cands.push(member_sch_tmpl);
                        }
                        let ty = Type::Var(phox.ctx.ty.fresh_var_id());
                        (ty.clone(), vec![Constraint::overloaded(&symbol, &ty, &cands)])
                    }
                }
            }
            // if let Some(tmpl) = phox.starlet_env.get_by_name(&symbol).first() {
            //     let sch = tmpl.fresh_copy(&mut phox.ctx);
            //     let (constraints, typed_starlet) = sch.instantiate(&mut phox.ctx);
            //     (typed_starlet.ty.clone(), constraints.into_vec())
            // }
            else {
                match icx.get_type_scheme(symbol) {
                    Some(sch) => {
                        let (constraints, ty) = sch.instantiate(&mut phox.ctx.ty);
                        (ty, constraints.into_vec())
                    }
                    None => {
                        let mut matches = Vec::new();
                        for tmpl in phox.impl_env.iter() {
                            if !tmpl.scheme_ref().target.members.iter().any(|(sym, _e, _ty)| sym == symbol) {
                                continue;
                            }
                            matches.push(tmpl.clone())
                        }
                        match matches.len() {
                            0 => return Err(Error::UnboundVariable(symbol.clone())),
                            1 => {
                                let tmpl = &matches.iter().cloned().next().unwrap();
                                let sch = &tmpl.fresh_copy(&mut phox.ctx.ty);
                                let member_sch = sch.get_member_scheme(symbol).unwrap();
                                let (constraints, ty_inst) = member_sch.instantiate(&mut phox.ctx.ty);
                                (ty_inst.clone(), constraints.into_vec())
                            }
                            _ => {
                                let symbol = symbol.clone();
                                let mut cands = Vec::new();
                                for sch_tmpl in &matches {
                                    let sch = sch_tmpl.fresh_copy(&mut phox.ctx.ty);
                                    let member_sch = sch.get_member_scheme(&symbol).unwrap();
                                    let member_sch_tmpl = SchemeTemplate::new(member_sch);
                                    cands.push(member_sch_tmpl);
                                }
                                let cands: Vec<_> = cands.iter().cloned().collect();
                                let ty = Type::Var(phox.ctx.ty.fresh_var_id());
                                (ty.clone(), vec![Constraint::overloaded(&symbol, &ty, &cands)])
                            }
                        }
                    }
                }
            }
        }

        ExprBody::App(f, a) => {
            let mut cs = Vec::new();
            let (ta, ca) = infer_expr(phox, module, icx, a)?;
            let (tf, cf) = infer_expr(phox, module, icx, f)?;
            let tr = Type::Var(phox.ctx.ty.fresh_var_id());
            cs.extend(ca);
            cs.extend(cf);
            cs.push(Constraint::type_eq(&tf, &Type::fun(ta, tr.clone())));
            (tr, cs)
        }

        ExprBody::Abs(pat, body) => {
            // パターンに対応する型を生成
            let t_pat = phox.ctx.ty.fresh_type_for_pattern(&pat);

            // 環境を拡張
            let mut icx2 = icx.duplicate();
            let monotype_bindings = true;
            let mut binndings = Vec::new();
            let mut cs = phox.ctx.ty.match_pattern(&mut icx2, &pat, &t_pat, monotype_bindings, &mut binndings)?;

            // 本体を推論
            let (t_body, cbody) = infer_expr(phox, module, &mut icx2, body)?;
            cs.extend(cbody);

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
            let mut css = Vec::new();

            // 判別対象式の型を推論
            let (t_scrut, c_scrut) = infer_expr(phox, module, icx, scrutinee)?;
            css.extend(c_scrut);

            // 各アームの式型を集める
            let mut result_types = vec![];
            for (pat, body) in arms.iter_mut() {
                // パターンに対応する型を生成（型変数を含む構造）
                let t_pat = phox.ctx.ty.fresh_type_for_pattern(&pat);

                // scrutinee の型とパターン型を unify
                css.push(Constraint::type_eq(&t_scrut, &t_pat));

                // 束縛環境を構築
                let mut icx2 = icx.duplicate();
                let monotype_bindings = true;
                let mut binndings = Vec::new();
                let cs = phox.ctx.ty.match_pattern(&mut icx2, &pat, &t_pat, monotype_bindings, &mut binndings)?;
                css.extend(cs);

                // アーム本体の型を推論
                let (t_body, c_body) = infer_expr(phox, module, &mut icx2, body)?;
                result_types.push(t_body);
                css.extend(c_body);
            }

            let mut result_types = result_types.into_iter();
            let t_result = result_types.next().ok_or(Error::EmptyMatch)?;
            for ty in result_types {
                css.push(Constraint::type_eq(&t_result, &ty));
            }

            (t_result, css)
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
                            Item::stmt(Stmt::Let(pat, p)),
                            Item::expr(Expr::field_access(Expr::unresolved_var("r"), field.clone()))
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
                            Item::stmt(Stmt::Let(pat, p)),
                            Item::expr(Expr::tuple_access(Expr::unresolved_var("t"), *index))
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
        ExprBody::TraitRecord(head) => {
            // eprintln!("TraitRecord({:?})", head);
            // eprintln!("  {}", head.pretty());
            match trait_members(phox, module, head) {
                None => return Err(Error::UnknownTrait(head.name.clone())),
                Some(members) => {
                    let mut cs = Vec::new();
                    let mut typed_fields = Vec::with_capacity(members.len());
                    for (field_name, field_sch_tmpl) in members.iter() {
                        let ty = Type::Var(phox.ctx.ty.fresh_var_id());
                        typed_fields.push((field_name.clone(), ty.clone()));
                        let sch = field_sch_tmpl.fresh_copy(&mut phox.ctx.ty);
                        let (constraints, ty_inst) = sch.instantiate(&mut phox.ctx.ty);
                        // cs.extend(constraints.into_vec());
                        cs.extend(constraints.into_vec_for_trait_record()); // <<- primary を除外（Constraint::TraitBound の重複を避けるため）
                        cs.push(Constraint::type_eq(&ty, &ty_inst));

                        // ★ primary と head が同型であることを要求する
                        if let Some(p) = &constraints.primary {
                            let hd = *p.clone();
                            // eprintln!("  req. {} == {}", hd, head);
                            for (t1, t2) in hd.params.iter().zip(head.params.iter()) {
                                cs.push(Constraint::type_eq(t1, t2));
                            }
                        }

                    }
                    cs.push(Constraint::trait_bound(head));
                    (Type::Record(typed_fields), cs)
                }
            }
        }
    };
    expr.ty = Some(ty.clone());
    Ok((ty, cs))
}

fn trait_members(
    phox: &mut PhoxEngine,
    module: &RefModule,
    head: &TraitHead,
) -> Option<IndexMap<String, SchemeTemplate<Type>>> {
    let Symbol::Unique(path) = &head.name else { return None };
    match path.resolve(module, &phox.roots) {
        Some((m, Some(rem))) if rem.len() == 1 => {
            let trait_members = m.borrow().trait_members.clone();
            let Some(member_names) = trait_members.get(&rem.to_string()) else { return None };
            let icx = &mut phox.get_infer_ctx(&m);
            let mut member_map = IndexMap::new();
            for member_name in member_names.iter() {
                let Some(tmpls) = icx.get_trait_member_schemes(&Symbol::trait_member(member_name.clone())) else { return None };
                for tmpl in tmpls.iter() {
                    let Some(trait_head) = tmpl.scheme_ref().constraints.primary.clone() else { continue };
                    if trait_head.name == head.name {
                        member_map.insert(member_name.clone(), tmpl.clone());
                    }
                }
            }
            Some(member_map)
        }
        _ => None,
    }
}

fn is_tycon(mut t: &Type) -> Option<Symbol> {
    while let Type::App(a, _) = t {
        t = a.as_ref_type();
    }
    match t {
        Type::Con(con) => Some(con.clone()),
        _ => None,
    }
}
