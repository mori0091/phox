use std::collections::HashMap;

use crate::api::PhoxEngine;
use crate::resolve::resolve_expr;
use crate::syntax::ast::*;
use crate::typesys::*;
use crate::module::*;

pub fn apply_trait_impls_item(
    phox: &mut PhoxEngine,
    module: &RefModule,
    item: &mut Item,
) -> Result<(), Error> {
    match &mut item.body {
        ItemBody::Decl(_) => Ok(()), // no need to apply trait impls
        ItemBody::Stmt(stmt) => apply_trait_impls_stmt(phox, module, stmt),
        ItemBody::Expr(expr) => apply_trait_impls_expr(phox, module, expr),
    }
}

pub fn apply_trait_impls_stmt(
    phox: &mut PhoxEngine,
    module: &RefModule,
    stmt: &mut Stmt,
) -> Result<(), Error> {
    match stmt {
        Stmt::Use(_) => Ok(()),
        Stmt::Mod(name, opt_items) => {
            let Some(items) = opt_items else { unreachable!() };
            let sub = &module.get_submod(name).unwrap();
            for item in items.iter_mut() {
                apply_trait_impls_item(phox, sub, item)?;
            }
            Ok(())
        }
        Stmt::Let(_pat, expr) => apply_trait_impls_expr(phox, module, expr),
        Stmt::LetRec(_pat, expr) => apply_trait_impls_expr(phox, module, expr),
    }
}

pub fn apply_trait_impls_expr(
    phox: &mut PhoxEngine,
    module: &RefModule,
    expr: &mut Expr,
) -> Result<(), Error> {
    let ty = expr.ty.as_ref().ok_or_else(|| Error::MissingType(Symbol::Local(expr.to_string())))?;
    // {
    //     eprintln!("[apply] {}: {}", expr, ty.repr(&mut phox.ctx));
    // }

    // 再帰的に子ノードを処理
    match &mut expr.body {
        ExprBody::App(f, a) => {
            apply_trait_impls_expr(phox, module, f)?;
            apply_trait_impls_expr(phox, module, a)?;
        }
        ExprBody::Abs(_, body) => {
            apply_trait_impls_expr(phox, module, body)?;
        }
        ExprBody::If(cond, then_, else_) => {
            apply_trait_impls_expr(phox, module, cond)?;
            apply_trait_impls_expr(phox, module, then_)?;
            apply_trait_impls_expr(phox, module, else_)?;
        }
        ExprBody::Match(scrutinee, arms) => {
            apply_trait_impls_expr(phox, module, scrutinee)?;
            for (_, arm_expr) in arms.iter_mut() {
                apply_trait_impls_expr(phox, module, arm_expr)?;
            }
        }
        ExprBody::Tuple(es) => {
            for e in es.iter_mut() {
                apply_trait_impls_expr(phox, module, e)?;
            }
        }
        ExprBody::Record(fields) => {
            for (_, e) in fields.iter_mut() {
                apply_trait_impls_expr(phox, module, e)?;
            }
        }
        ExprBody::TupleAccess(e, _) | ExprBody::FieldAccess(e, _) => {
            apply_trait_impls_expr(phox, module, e)?;
        }
        ExprBody::Block(items) => {
            for item in items.iter_mut() {
                apply_trait_impls_item(phox, module, item)?;
            }
        }
        ExprBody::Var(name) => {
            if phox.get_infer_ctx(module).is_trait_member(name) {
                let mut matches = Vec::new();
                for tmpl in phox.impl_env.iter() {
                    if !tmpl.scheme_ref().target.members.iter().any(|(sym, _e, _ty)| sym == name) {
                        continue;
                    }
                    let mut try_ctx = phox.ctx.clone();
                    let sch = tmpl.fresh_copy(&mut try_ctx);
                    let (_constraints, typed_impl) = sch.instantiate(&mut try_ctx);
                    let (_sym, _e, ty_inst) = typed_impl.members.iter().find(|(sym, _e, _ty)| sym == name).unwrap();
                    if try_ctx.unify(ty_inst, ty).is_ok() {
                        matches.push(tmpl.clone());
                    }
                }
                match matches.len() {
                    0 => {
                        // 実装が見つからない → 何もしない
                        // 通常の変数参照なら infer_expr 側で処理される
                    }
                    1 => {
                        let tmpl = matches.iter().next().unwrap();
                        let sch = tmpl.fresh_copy(&mut phox.ctx);
                        let (constraints, typed_impl) = sch.instantiate(&mut phox.ctx);
                        let (_sym, expr_inst, ty_inst) = typed_impl.members.iter().find(|(sym, _e, _ty)| sym == name).unwrap();
                        let mut cs = constraints.into_vec();
                        cs.push(Constraint::type_eq(ty_inst, ty));
                        solve(phox, cs)?;
                        // This `expr_inst` would be a type-solved expression!
                        let mut expr_inst = expr_inst.clone();
                        {
                            // NOTE:
                            //
                            // The inference and application (baking) of `impl`
                            // member implementations occurs within the
                            // environment of the module where the
                            // implementation is defined.
                            //
                            // However, since the expression may reference
                            // symbols not exported to the calling environment,
                            // it currently needs to be resolved again in the
                            // calling environment.
                            let symbol_env = &mut phox.get_symbol_env(module);
                            let param_map = &mut HashMap::new();
                            resolve_expr(phox, module, symbol_env, param_map, &mut expr_inst)?;

                            apply_trait_impls_expr(phox, module, &mut expr_inst)?;
                        }
                        expr.body = expr_inst.body;
                        // expr.ty は既に推論済みなのでそのままでOK
                    }
                    _ => {
                        for tmpl in &matches {
                            eprintln!("{}", tmpl.scheme_ref().pretty());
                        }
                        panic!();
                    }
                }
            }
        }
        ExprBody::RawTraitRecord(_) => {
            unreachable!()
        }
        ExprBody::TraitRecord(head) => {
            // eprintln!("[apply] @{{{}}}: {}", head, ty.repr(&mut phox.ctx));

            let body = make_record_from_trait(phox, head, &ty.clone())?;
            expr.body = body;

            let symbol_env = &mut phox.get_symbol_env(module);
            let param_map = &mut HashMap::new();
            resolve_expr(phox, module, symbol_env, param_map, expr)?;

            apply_trait_impls_expr(phox, module, expr)?;
        }
        ExprBody::Lit(_) => {}
    }
    // eprintln!("apply_trait_impls_expr (done)");

    Ok(())
}

pub fn make_record_from_trait(
    phox: &mut PhoxEngine,
    head: &TraitHead,
    ty: &Type,
) -> Result<ExprBody, Error> {
    let mut matches = Vec::new();
    for tmpl in &phox.impl_env.get_by_name(&head.name) {
        let dummy_ctx = &mut phox.ctx.clone();
        let sch = tmpl.fresh_copy(dummy_ctx);
        let (_constraints, imp) = sch.instantiate(dummy_ctx);
        let fields: Vec<(String, Type)> = imp
            .members
            .iter()
            .map(|(sym, _e, t)| (sym.to_string(), t.clone()))
            .collect();
        let ty_inst = Type::Record(fields);
        if dummy_ctx.unify(&ty_inst, ty).is_ok() {
            matches.push(tmpl.clone());
        }
    }
    match matches.len() {
        0 => {
            // 実装が見つからない
            Err(Error::MissingImpl(head.clone()))
        }
        1 => {
            let sch = matches[0].fresh_copy(&mut phox.ctx);
            let (constraints, imp) = sch.instantiate(&mut phox.ctx);

            let fields_ty: Vec<(String, Type)> = imp
                .members
                .iter()
                .map(|(sym, _e, t)| (sym.to_string(), t.clone()))
                .collect();
            let ty_inst = Type::Record(fields_ty);

            let mut cs = constraints.into_vec();
            cs.push(Constraint::type_eq(&ty_inst, ty));
            solve(phox, cs)?;

            let fields: Vec<(String, Expr)> = imp
                .members
                .iter()
                .map(|(sym, e, _t)| (sym.to_string(), e.clone()))
                .collect();

            Ok(ExprBody::Record(fields))
        }
        _ => {
            eprintln!("** make_record_from_trait ** {:?}", head);
            let dummy_ctx = &mut phox.ctx.clone();
            let cands: Vec<_> = matches.into_iter().map(
                |tmpl| {
                    let sch = tmpl.fresh_copy(dummy_ctx);
                    Scheme {
                        vars: vec![],
                        constraints: sch.constraints,
                        target: sch.target.head,
                    }
                }
            ).collect();
            Err(Error::AmbiguousTrait {
                trait_head: head.clone(),
                candidates: cands,
            })
        }
    }
}
