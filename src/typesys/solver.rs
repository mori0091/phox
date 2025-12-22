use crate::api::PhoxEngine;
use crate::syntax::ast::*;
use crate::module::*;
use super::*;

pub fn solve(
    phox: &mut PhoxEngine,
    constraints: Vec<Constraint>,
) -> Result<(), Error> {
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
                },
                Constraint::TraitBound(head) => {
                    let cs = solve_trait_bound(phox, &head)?;
                    worklist.extend(cs); // <<< OK. or, probably, `pending.extend(cs)` is also OK.
                    changed = true;
                },
                Constraint::Overloaded(sym, ty, sch_tmpls) => {
                    let repr_ty = ty.repr(&mut phox.ctx);
                    if repr_ty != ty {
                        changed = true;
                    }
                    let cs = solve_overloaded(phox, &sym, &repr_ty, &sch_tmpls)?;
                    // worklist.extend(cs);  // <<< NG. Don't this! This may causes inifinite loop!!
                    pending.extend(cs); // <<< OK!
                },
            }
        }
    }

    if !pending.is_empty() {
        let e = match pending[0].clone() {
            Constraint::TypeEq(t1, t2) => {
                Error::TypeMismatch(t1, t2)
            },
            Constraint::TraitBound(head) => {
                Error::AmbiguousTrait {
                    trait_head: head,
                    candidates: vec![]
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
            let impl_head_sch = &candidates[0];
            let (constraints, impl_head) = impl_head_sch.instantiate(&mut phox.ctx);
            let mut cs = head.params
                             .iter()
                             .zip(impl_head.params.iter())
                             .map(|(t1, t2)| Constraint::type_eq(t1, t2))
                             .collect::<Vec<_>>();
            cs.extend(constraints.into_vec());
            Ok(cs)
        }
        _ => Err(Error::AmbiguousTrait {
            trait_head: head.clone(),
            candidates,
        }),
    }
}

fn lookup_impls_by_trait_head(
    phox: &mut PhoxEngine,
    head: &TraitHead
) -> Vec<Scheme<TraitHead>> {
    let mut candidates = Vec::new();
    for impl_head_sch in phox.impl_env.keys() {
        if head.name == impl_head_sch.target.name
            && head.params.len() == impl_head_sch.target.params.len()
        {
            let ctx2 = &mut phox.ctx.clone();
            let (_, impl_head) = impl_head_sch.instantiate(ctx2);
            let ok = head.params
                         .iter()
                         .zip(impl_head.params.iter())
                         .all(|(t1, t2)| ctx2.unify(t1, t2).is_ok());
            if ok {
                candidates.push(impl_head_sch.clone())
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

    let (best_score, winner_tmpl) = filtered
        .iter()
        .max_by_key(|(score, _)| score)
        .unwrap();
    {
        let candidates: Vec<_> = filtered
            .iter()
            .filter(|(score, _)| score == best_score)
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
    Ok(constraints.into_vec())
}
