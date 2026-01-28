use indexmap::IndexSet;

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
    let mut worklist = Vec::new();
    let mut pending = constraints;
    let mut changed = true;
    while changed {
        worklist.extend(pending.drain(..));
        changed = false;
        while let Some(constraint) = worklist.pop() {
            match constraint.solve(phox)? {
                Response::Done => {
                    changed = true;
                }
                Response::Progressed(cs) => {
                    changed = true;
                    pending.extend(cs);
                }
                Response::Defered(cs) => {
                    pending.extend(cs);
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
        Err(pending[0].clone().error(phox))
    }
    else {
        Ok(())
    }
}

// -------------------------------------------------------------
pub enum Response<T> {
    Done,
    Progressed(T),
    Defered(T),
}

pub type SolverResult = Result<Response<Vec<Constraint>>, Error>;

pub trait Solver {
    fn solve(self, phox: &mut PhoxEngine) -> SolverResult;
    fn error(self, phox: &mut PhoxEngine) -> Error;
}

// -------------------------------------------------------------
impl Solver for Constraint {
    fn solve(self, phox: &mut PhoxEngine) -> SolverResult {
        match self {
            Constraint::Ty(c) => c.solve(phox),
            // Constraint::Row(c) => c.solve(phox),
        }
    }
    fn error(self, phox: &mut PhoxEngine) -> Error {
        match self {
            Constraint::Ty(c) => c.error(phox),
            // Constraint::Row(c) => c.error(phox),
        }
    }
}

// -------------------------------------------------------------
impl Solver for TypeConstraint {
    fn solve(self, phox: &mut PhoxEngine) -> SolverResult {
        match &self {
            TypeConstraint::TypeEq(t1, t2) => {
                phox.ctx.ty.unify(&t1, &t2)?;
                Ok(Response::Done)
            }
            TypeConstraint::TraitBound(head) => {
                let mut fv = IndexSet::new();
                head.free_vars(&mut phox.ctx, &mut fv);

                if fv.iter().any(|v| phox.ctx.non_touchable.contains(v)) {
                    Ok(Response::Defered(vec![Constraint::Ty(self)]))
                }
                else if let Ok(cs) = solve_trait_bound(phox, &head) {
                    Ok(Response::Progressed(cs))
                }
                else {
                    Ok(Response::Defered(vec![Constraint::Ty(self)]))
                }
            }
            TypeConstraint::Overloaded(sym, ty, sch_tmpls) => {
                let repr_ty = ty.repr(&mut phox.ctx.ty);
                let cs = solve_overloaded(phox, &sym, &repr_ty, &sch_tmpls)?;
                if repr_ty != *ty {
                    Ok(Response::Progressed(cs))
                }
                else {
                    Ok(Response::Defered(cs))
                }
            }
        }
    }

    fn error(self, phox: &mut PhoxEngine) -> Error {
        match self {
            TypeConstraint::TypeEq(t1, t2) => {
                Error::TypeMismatch(t1, t2)
            },
            TypeConstraint::TraitBound(head) => {
                let candidates = lookup_impls_by_trait_head(phox, &head);
                if candidates.is_empty() {
                    Error::MissingImpl(head)
                }
                else {
                    let mut dummy_ctx = phox.ctx.clone();
                    let cands = candidates.iter().map(
                        |tmpl| {
                            let sch = tmpl.fresh_copy(&mut dummy_ctx.ty);
                            Scheme {
                                vars: vec![],
                                constraints: sch.constraints.clone(),
                                target: sch.target.head.clone()
                            }
                        }
                    ).collect();
                    Error::AmbiguousImpl {
                        trait_head: head,
                        candidates: cands,
                    }
                }
            },
            TypeConstraint::Overloaded(sym, ty, sch_tmpls) => {
                if ty.contains_type_var() {
                    Error::AmbiguousVariable {
                        name: sym,
                        candidates: sch_tmpls
                            .iter()
                            .map(|tmpl| tmpl.fresh_copy(&mut phox.ctx.ty))
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
        }
    }
}

// -------------------------------------------------------------
fn solve_trait_bound(
    phox: &mut PhoxEngine,
    head: &TraitHead
) -> Result<Vec<Constraint>, Error> {
    let candidates = lookup_impls_by_trait_head(phox, &head);
    match candidates.len() {
        0 => Err(Error::MissingImpl(head.clone())),
        1 => {
            let tmpl = &candidates[0];
            let sch = tmpl.fresh_copy(&mut phox.ctx.ty);
            let (constraints, typed_impl) = sch.instantiate(&mut phox.ctx.ty);
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
            let mut dummy_ctx = phox.ctx.clone();
            let cands = candidates.iter().map(
                |tmpl| {
                    let sch = tmpl.fresh_copy(&mut dummy_ctx.ty);
                    Scheme {
                        vars: vec![],
                        constraints: sch.constraints.clone(),
                        target: sch.target.head.clone()
                    }
                }
            ).collect();
            Err(Error::AmbiguousImpl {
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
            let mut try_ctx = phox.ctx.clone();
            let sch = tmpl.fresh_copy(&mut try_ctx.ty);
            let (_, typed_impl) = sch.instantiate(&mut try_ctx.ty);
            let ok = head.params
                .iter()
                .zip(typed_impl.head.params.iter())
                .all(|(t1, t2)| try_ctx.ty.unify(t1, t2).is_ok());
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
    let mut candidates = Vec::new();
    for cand_tmpl in sch_tmpls {
        let mut try_ctx = phox.ctx.clone();
        let cand = cand_tmpl.fresh_copy(&mut try_ctx.ty);
        let (_, ty_inst) = &cand.instantiate(&mut try_ctx.ty);
        if try_ctx.ty.unify(ty, ty_inst).is_ok() {
            candidates.push(cand_tmpl);
        }
    }

    if candidates.is_empty() {
        return Err(Error::NoMatchingOverload(Expr {
            span: (0,0), // \TODO unused yet
            body: ExprBody::Var(sym.clone()),
            ty: Some(ty.clone()),
        }));
    }

    if candidates.len() > 1 {
        let tmpls = candidates
            .into_iter()
            .map(|tmpl| tmpl.clone())
            .collect::<Vec<_>>();
        return Ok(vec![Constraint::overloaded(sym, ty, &tmpls)]);
    }

    let winner = candidates[0].fresh_copy(&mut phox.ctx.ty);
    let (constraints, ty_inst) = winner.instantiate(&mut phox.ctx.ty);
    phox.ctx.ty.unify(ty, &ty_inst)?;
    // Ok(constraints.into_vec())
    Ok(constraints.into_vec_for_trait_record()) // Vec<Constraint> w/o ConstraintSet::primary
}
