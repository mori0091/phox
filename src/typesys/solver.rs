use std::collections::HashSet;
use std::hash::Hash;

fn intersects<T: Eq + Hash>(a: &HashSet<T>, b: &HashSet<T>) -> bool {
    a.intersection(b).next().is_some()
}

use crate::api::PhoxEngine;
use crate::syntax::ast::*;
use crate::module::*;
use super::*;

pub fn solve(
    phox: &mut PhoxEngine,
    constraints: Vec<Constraint>,
) -> Result<(), Error> {
    let pending = solve_with_residual(phox, constraints)?;
    solve_error(phox, pending)
}

pub fn solve_with_residual(
    phox: &mut PhoxEngine,
    constraints: Vec<Constraint>,
) -> Result<Vec<Constraint>, Error> {
    let mut worklist = constraints;
    let mut pending = Vec::new();
    let mut changed = true;
    while changed {
        worklist.extend(pending.drain(..));
        changed = false;
        while let Some(c) = worklist.pop() {
            match c {
                Constraint::TypeEq(t1, t2) => {
                    phox.ctx.unify(&t1, &t2)?;
                    changed = true;
                }
                Constraint::TraitBound(head) => {
                    let mut fv = HashSet::new();
                    head.free_type_vars(&mut phox.ctx, &mut fv);

                    if intersects(&fv, &phox.ctx.non_touchable) {
                        pending.push(Constraint::TraitBound(head));
                    }
                    else if let Ok(cs) = solve_trait_bound(phox, &head) {
                        pending.extend(cs); // <<< OK. `pending.extend(cs)` is better than `worklist.extend(cs)`.
                        changed = true;
                    }
                    else {
                        pending.push(Constraint::TraitBound(head));
                    }
                }
                Constraint::Overloaded(sym, ty, sch_tmpls) => {
                    let repr_ty = ty.repr(&mut phox.ctx);
                    if repr_ty != ty {
                        changed = true;
                    }
                    let cs = solve_overloaded(phox, &sym, &repr_ty, &sch_tmpls)?;
                    // worklist.extend(cs);  // <<< NG. Don't this! This may causes inifinite loop!!
                    pending.extend(cs); // <<< OK!
                }
            }
        }
    }
    Ok(pending)
}

pub fn solve_error(
    phox: &mut PhoxEngine,
    pending: Vec<Constraint>,
) -> Result<(), Error> {
    if !pending.is_empty() {

        // eprintln!("[solve_error] pending from here:");
        // for c in &pending {
        //     eprintln!("  {}", c);
        // }

        let e = match pending[0].clone() {
            Constraint::TypeEq(t1, t2) => {
                Error::TypeMismatch(t1, t2)
            },
            Constraint::TraitBound(head) => {
                let candidates = lookup_impls_by_trait_head(phox, &head);
                if candidates.is_empty() {
                    Error::MissingImpl(head)
                }
                else {
                    let dummy_ctx = &mut phox.ctx.clone();
                    let cands = candidates.iter().map(
                        |tmpl| {
                            let sch = tmpl.fresh_copy(dummy_ctx);
                            Scheme {
                                vars: vec![],
                                constraints: sch.constraints.clone(),
                                target: sch.target.head.clone()
                            }
                        }
                    ).collect();
                    Error::AmbiguousTrait {
                        trait_head: head,
                        candidates: cands,
                    }
                }
            },
            Constraint::Overloaded(sym, ty, sch_tmpls) => {
                if ty.contains_type_var() {
                    Error::AmbiguousVariable {
                        name: sym,
                        candidates: sch_tmpls
                            .iter()
                            .map(|tmpl| tmpl.fresh_copy(&mut phox.ctx))
                            .collect(),
                    }
                }
                else {
                    Error::NoMatchingOverload(Expr {
                        span: (0,0), // \TODO unused yet
                        body: ExprBody::Var(sym),
                        ty: Some(ty),
                    })
                }
            },
        };
        return Err(e)
    }

    Ok(())
}

fn solve_trait_bound(
    phox: &mut PhoxEngine,
    head: &TraitHead
) -> Result<Vec<Constraint>, Error> {
    let candidates = lookup_impls_by_trait_head(phox, &head);
    match candidates.len() {
        0 => Err(Error::MissingImpl(head.clone())),
        1 => {
            let tmpl = &candidates[0];
            let sch = tmpl.fresh_copy(&mut phox.ctx);
            let (constraints, typed_impl) = sch.instantiate(&mut phox.ctx);
            let mut cs = head.params
                             .iter()
                             .zip(typed_impl.head.params.iter())
                             .map(|(t1, t2)| Constraint::type_eq(t1, t2))
                             .collect::<Vec<_>>();
            cs.extend(constraints.into_vec());
            Ok(cs)
        }
        _ => {
            // eprintln!("** solve_trait_bound ** {:?}", head);
            let dummy_ctx = &mut phox.ctx.clone();
            let cands = candidates.iter().map(
                |tmpl| {
                    let sch = tmpl.fresh_copy(dummy_ctx);
                    Scheme {
                        vars: vec![],
                        constraints: sch.constraints.clone(),
                        target: sch.target.head.clone()
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

fn lookup_impls_by_trait_head(
    phox: &mut PhoxEngine,
    head: &TraitHead
) -> Vec<SchemeTemplate<TypedImpl>> {
    let mut candidates = Vec::new();
    for tmpl in phox.impl_env.iter() {
        let sch = tmpl.scheme_ref();
        if head.name == sch.target.head.name
            && head.params.len() == sch.target.head.params.len()
        {
            let try_ctx = &mut phox.ctx.clone();
            let sch = tmpl.fresh_copy(try_ctx);
            let (_, typed_impl) = sch.instantiate(try_ctx);
            let ok = head.params
                .iter()
                .zip(typed_impl.head.params.iter())
                .all(|(t1, t2)| try_ctx.unify(t1, t2).is_ok());
            if ok {
                candidates.push(tmpl.clone())
            }
        }
    }
    candidates
}

fn solve_overloaded(
    phox: &mut PhoxEngine,
    sym: &Symbol,
    ty: &Type,
    sch_tmpls: &Vec<SchemeTemplate<Type>>,
) -> Result<Vec<Constraint>, Error> {
    let mut filtered = Vec::new();
    for cand_tmpl in sch_tmpls {
        let mut try_ctx = phox.ctx.clone();
        let cand = cand_tmpl.fresh_copy(&mut try_ctx);
        let (_, ty_inst) = &cand.instantiate(&mut try_ctx);
        if try_ctx.unify(ty, ty_inst).is_ok() {
            filtered.push((cand.target.score(), cand_tmpl));
        }
    }

    if filtered.is_empty() {
        return Err(Error::NoMatchingOverload(Expr {
            span: (0,0), // \TODO unused yet
            body: ExprBody::Var(sym.clone()),
            ty: Some(ty.clone()),
        }));
    }

    let (_best_score, winner_tmpl) = filtered
        .iter()
        .max_by_key(|(score, _)| score)
        .unwrap();
    {
        let candidates: Vec<_> = filtered
            .iter()
            // .filter(|(score, _)| score == best_score)
            .cloned()
            .collect();
        if candidates.len() > 1 {
            let tmpls = filtered
                .into_iter()
                .map(|(_, tmpl)| tmpl)
                .cloned()
                .collect();
            return Ok(vec![Constraint::Overloaded(sym.clone(), ty.clone(), tmpls)]);
        }
    }

    let winner = winner_tmpl.fresh_copy(&mut phox.ctx);
    let (constraints, ty_inst) = winner.instantiate(&mut phox.ctx);
    phox.ctx.unify(ty, &ty_inst)?;
    // Ok(constraints.into_vec())
    Ok(constraints.into_vec_for_trait_record()) // Vec<Constraint> w/o ConstraintSet::primary
}
