use crate::error::*;
use crate::module::*;
use crate::syntax::ast;
use crate::coreir::*;
use crate::vm;

pub struct VMIR {
    pub globals: vm::GlobalEnv,
    pub code: vm::Code,
}

impl VMIR {
    pub fn new() -> VMIR {
        VMIR {
            globals: vm::GlobalEnv::new(),
            code: vm::Code::unit(),
        }
    }

    pub fn lower(&mut self, c: &CoreIR) -> Result<(), Error> {
        for (sym, e) in &c.globals {
            let t = lower_expr(&mut vec![], e)?;
            self.globals.insert(sym.clone(), t);
        }
        let mut es = Vec::new();
        for e in &c.expressions {
            let e = lower_expr(&mut vec![], e)?;
            es.push(e);
        }
        self.code = vm::Code::block(es);
        Ok(())
    }
}

fn de_bruijn_index_push(env: &mut Vec<Symbol>, v: &Symbol) {
    env.push(v.clone());
}

fn de_bruijn_index_lookup(env: &Vec<Symbol>, v: &Symbol) -> Result<usize, Error> {
    env.iter()
       .rev()
       .position(|s| v == s)
       .ok_or_else(|| Error::UnboundVariable(v.clone()))
}

fn lower_pat(env: &mut Vec<Symbol>, p: &ast::Pat) -> vm::Pat {
    match p {
        ast::Pat::Wildcard => {
            vm::Pat::Wildcard
        }
        ast::Pat::Var(v) => {
            de_bruijn_index_push(env, v);
            vm::Pat::Var
        }
        ast::Pat::Lit(x) => {
            vm::Pat::Lit(x.clone())
        }
        ast::Pat::Con(name, xs) => {
            let xs: Vec<_> = xs.iter().map(|p| lower_pat(env, p)).collect();
            vm::Pat::Con(name.clone(), xs)
        }
        ast::Pat::Tuple(xs) => {
            let xs: Vec<_> = xs.iter().map(|p| lower_pat(env, p)).collect();
            vm::Pat::Tuple(xs)
        }
        ast::Pat::Record(fs) => {
            let fs: Vec<_> = fs.iter().map(|(k, p)| (k.clone(), lower_pat(env, p))).collect();
            vm::Pat::Record(fs)
        }
    }
}

fn lower_expr(env: &mut Vec<Symbol>, expr: &CoreExpr) -> Result<vm::Code, Error> {
    match expr {
        CoreExpr::Let(p, x, e) => {
            let x = lower_expr(&mut env.clone(), x)?;
            let p = lower_pat(env, p);
            let e = lower_expr(env, e)?;
            Ok(vm::Code::let_p0(p, x, e))
        }
        CoreExpr::LetRec(s, x, e) => {
            de_bruijn_index_push(env, s);
            let x = lower_expr(&mut env.clone(), x)?;
            let e = lower_expr(env, e)?;
            Ok(vm::Code::letrec(x, e))
        }

        CoreExpr::GlobalVar(sym) => {
            Ok(vm::Code::GlobalVar(sym.clone()))
        }
        CoreExpr::Var(sym) => {
            let index = de_bruijn_index_lookup(env, sym)?;
            Ok(vm::Code::Var(index))
        }

        CoreExpr::Lam(pat, expr) => {
            match pat {
                ast::Pat::Var(v) => {
                    de_bruijn_index_push(env, v);
                    let e = lower_expr(env, expr)?;
                    Ok(vm::Code::lam(e))
                }
                _ => {
                    de_bruijn_index_push(env, &Symbol::local("?"));
                    let p = lower_pat(env, pat);
                    let e = lower_expr(env, expr)?;
                    Ok(vm::Code::lam_p1(p, e))
                }
            }
        }
        CoreExpr::App(f, x) => {
            let f = lower_expr(&mut env.clone(), f)?;
            let x = lower_expr(env, x)?;
            Ok(vm::Code::app(f, x))
        }
        CoreExpr::Builtin(b) => {
            Ok(vm::Code::Builtin(b.clone()))
        }
        CoreExpr::Match(scrut, arms) => {
            let scrut = lower_expr(env, scrut)?;
            let mut xs = Vec::with_capacity(arms.len());
            for (p, e) in arms {
                let env = &mut env.clone();
                let p = lower_pat(env, p);
                let e = lower_expr(env, e)?;
                xs.push((p, e));
            }
            Ok(vm::Code::match_(scrut, xs))
        }
        CoreExpr::For(init, pred, next) => {
            let init = lower_expr(&mut env.clone(), init)?;
            let pred = lower_expr(&mut env.clone(), pred)?;
            let next = lower_expr(env, next)?;
            Ok(vm::Code::for_(init, pred, next))
        }

        CoreExpr::TupleAccess(t, index) => {
            let t = lower_expr(env, t)?;
            Ok(vm::Code::tuple_access(t, *index))
        }
        CoreExpr::FieldAccess(r, label) => {
            let r = lower_expr(env, r)?;
            Ok(vm::Code::field_access(r, label.clone()))
        }

        CoreExpr::Lit(x) => {
            Ok(vm::Code::Lit(x.clone()))
        }
        CoreExpr::Con(name, xs) => {
            let mut args = Vec::with_capacity(xs.len());
            for x in xs {
                let e = lower_expr(&mut env.clone(), x)?;
                args.push(e);
            }
            let t = vm::Code::Con(name.clone(), args.len());
            Ok(vm::Code::clo(t, args))
        }
        CoreExpr::Tuple(xs) => {
            let mut args = Vec::with_capacity(xs.len());
            for x in xs {
                let e = lower_expr(&mut env.clone(), x)?;
                args.push(e);
            }
            let t = vm::Code::Tuple(args.len());
            Ok(vm::Code::clo(t, args))
        }
        CoreExpr::Record(fs) => {
            let mut ix = Vec::with_capacity(fs.len());
            let mut args = Vec::with_capacity(fs.len());
            for (f, x) in fs {
                let e = lower_expr(&mut env.clone(), x)?;
                args.push(e);
                ix.push(f.clone())
            }
            let t = vm::Code::Record(ix);
            Ok(vm::Code::clo(t, args))
        }
    }
}
