use std::collections::{HashMap, HashSet};

use crate::syntax::ast::{Kind, Type, TypeVarId, Scheme, Expr, Lit, Pat};

// ===== Type error =====
#[derive(Debug)]
pub enum TypeError {
    UnboundVariable(String),
    Mismatch(Type, Type),
    TupleLengthMismatch(usize, usize),
    ExpectedTuple(Type),
    UnsupportedPattern(Pat),
    RecursiveType,
    UnknownConstructor(String),
    ConstructorArityMismatch(String, usize, Type),
    LetRecPatternNotSupported(Pat),
}

// ===== Type Environment =====
pub struct TypeEnv {
    pub kinds: HashMap<String, Kind>, // 型構築子の種類
}

// ===== Environment =====
#[derive(Default, Clone)]
pub struct Env {
    pub map: HashMap<String, Scheme>,
}

// ===== Type context: union-find + binding =====
pub struct TypeContext {
    parent: Vec<TypeVarId>,    // union-find parent pointers
    binding: Vec<Option<Type>>, // representative binding (Some if bound to a type)
}

impl TypeContext {
    pub fn new() -> Self {
        Self { parent: Vec::new(), binding: Vec::new() }
    }

    pub fn fresh_var(&mut self) -> TypeVarId {
        let id = TypeVarId(self.parent.len());
        self.parent.push(id);
        self.binding.push(None);
        id
    }
}

impl TypeContext {
    // find with path compression
    fn find(&mut self, id: TypeVarId) -> TypeVarId {
        let p = self.parent[id.0];
        if p != id {
            let root = self.find(p);
            self.parent[id.0] = root;
        }
        self.parent[id.0]
    }
}

impl TypeContext {
    // Occurs check: does tv occur in ty (following bindings)?
    fn occurs_in(&mut self, tv: TypeVarId, ty: &Type) -> bool {
        match ty {
            Type::Var(v) => {
                // find は短く終わらせる
                let r = self.find(*v);
                if r == tv {
                    return true;
                }
                // binding[r] への参照を保持しない。Option<Type> をクローンして借用を解放。
                let bound_opt = self.binding[r.0].clone();
                if let Some(bound) = bound_opt {
                    // 参照はすでに解放済みなので、ここで &mut self を再借用できる
                    self.occurs_in(tv, &bound)
                } else {
                    false
                }
            }
            Type::Fun(a, b) => {
                self.occurs_in(tv, a) || self.occurs_in(tv, b)
            }
            Type::Tuple(ts) => {
                ts.iter().any(|t| self.occurs_in(tv, t))
            }
            Type::Struct(_, _) => todo!(),
            _ => false,
        }
    }
}

impl TypeContext {
    // Normalize a type by chasing bindings and compressing vars
    fn repr(&mut self, ty: &Type) -> Type {
        match ty {
            Type::Var(v) => {
                let r = self.find(*v);
                // ここでも Option<Type> をクローンして借用を解放
                let bound_opt = self.binding[r.0].clone();
                if let Some(bound) = bound_opt {
                    self.repr(&bound)
                } else {
                    Type::Var(r)
                }
            }
            Type::Fun(a, b) => Type::fun(self.repr(a), self.repr(b)),
            Type::App(f, x) => Type::app(self.repr(f), self.repr(x)),
            Type::Con(c) => Type::con(c.clone()),
            Type::Tuple(ts) => Type::Tuple(ts.iter().map(|t| self.repr(t)).collect()),
            Type::Struct(_, _) => todo!(),
        }
    }
}

impl TypeContext {
    fn unify(&mut self, a: &Type, b: &Type) -> Result<(), TypeError> {
        let a = self.repr(a);
        let b = self.repr(b);
        match (&a, &b) {
            (Type::Var(v1), Type::Var(v2)) => {
                let r1 = self.find(*v1);
                let r2 = self.find(*v2);
                if r1 != r2 {
                    // union by attaching r1 under r2
                    self.parent[r1.0] = r2;
                }
                Ok(())
            }

            (Type::Var(v), t) | (t, Type::Var(v)) => {
                let r = self.find(*v);
                // t might reduce further; use repr to avoid deep chains
                let t = self.repr(t);
                if self.occurs_in(r, &t) {
                    Err(TypeError::RecursiveType)
                } else {
                    self.binding[r.0] = Some(t);
                    Ok(())
                }
            }

            (Type::Fun(a1, b1), Type::Fun(a2, b2)) => {
                self.unify(a1, a2)?;
                self.unify(b1, b2)
            }

            (Type::Con(c1), Type::Con(c2)) if c1 == c2 => Ok(()),

            (Type::App(f1, x1), Type::App(f2, x2)) => {
                self.unify(f1, f2)?;
                self.unify(x1, x2)
            }

            (Type::Tuple(ts1), Type::Tuple(ts2)) => {
                if ts1.len() != ts2.len() {
                    return Err(TypeError::TupleLengthMismatch(ts1.len(), ts2.len()));
                }
                for (t1, t2) in ts1.iter().zip(ts2.iter()) {
                    self.unify(t1, t2)?;
                }
                Ok(())
            }

            (Type::Struct(_s1, _fs1), Type::Struct(_s2, _fs2)) => {
                todo!()
            }
            (ta, tb) if ta == tb => Ok(()),
            _ => Err(TypeError::Mismatch(a, b)),
        }
    }
}

impl TypeContext {
    pub fn fresh_type_for_pattern(&mut self, pat: &Pat) -> Type {
        match pat {
            Pat::Var(_) | Pat::Wildcard => Type::Var(self.fresh_var()),
            Pat::Lit(lit) => match lit {
                Lit::Unit => Type::con("Unit"),
                Lit::Bool(_) => Type::con("Bool"),
                Lit::Int(_) => Type::con("Int"),
            },
            Pat::Tuple(ps) => {
                let ts = ps.iter().map(|p| self.fresh_type_for_pattern(p)).collect();
                Type::Tuple(ts)
            }
            Pat::Con(_, _) => {
                Type::Var(self.fresh_var()) // パターン全体の型は未知とする
            }
            Pat::Struct(_, _) => todo!(),
        }
    }
}

impl TypeContext {
    fn match_pattern(&mut self, env: &mut Env, pat: &Pat, ty: &Type) -> Result<(), TypeError> {
        match pat {
            Pat::Var(x) => {
                env.map.insert(x.clone(), Scheme::mono(ty.clone()));
                Ok(())
            }
            Pat::Wildcard => Ok(()), // 束縛なし
            Pat::Lit(lit) => {
                let expected = match lit {
                    Lit::Unit => Type::Con("Unit".to_string()),
                    Lit::Bool(_) => Type::Con("Bool".to_string()),
                    Lit::Int(_) => Type::Con("Int".to_string()),
                };
                if &expected == ty {
                    Ok(())
                } else {
                    Err(TypeError::Mismatch(expected, ty.clone()))
                }
            }
            Pat::Tuple(ps) => {
                match ty {
                    Type::Tuple(ts) => {
                        if ps.len() != ts.len() {
                            return Err(TypeError::TupleLengthMismatch(ps.len(), ts.len()));
                        }
                        for (p, t) in ps.iter().zip(ts.iter()) {
                            self.match_pattern(env, p, t)?;
                        }
                        Ok(())
                    }
                    _ => Err(TypeError::ExpectedTuple(ty.clone())),
                }
            }
            Pat::Con(name, args) => {
                let scheme = env.map.get(name).ok_or(TypeError::UnknownConstructor(name.clone()))?;
                let con_ty = instantiate(self, scheme);

                let mut arg_types = Vec::new();
                let mut ty_fun = con_ty;

                for _ in args {
                    match self.repr(&ty_fun) {
                        Type::Fun(a, b) => {
                            arg_types.push(*a);
                            ty_fun = *b;
                        }
                        other => return Err(TypeError::ConstructorArityMismatch(name.clone(), args.len(), other)),
                    }
                }

                self.unify(&ty_fun, ty)?;

                for (p, t) in args.iter().zip(arg_types.iter()) {
                    self.match_pattern(env, p, t)?;
                }

                Ok(())
            }
            Pat::Struct(_, _) => {
                // まだ未対応。構造体型の照合が必要
                Err(TypeError::UnsupportedPattern(pat.clone()))
            }
        }
    }
}

// ===== Free type variables =====
fn free_ty_vars(ctx: &mut TypeContext, ty: &Type, acc: &mut HashSet<TypeVarId>) {
    match ctx.repr(ty) {
        Type::Var(v) => {
            acc.insert(v);
        }
        Type::Fun(ref a, ref b) => {
            free_ty_vars(ctx, a, acc);
            free_ty_vars(ctx, b, acc);
        }
        Type::Con(_) => {}
        Type::App(ref a, ref b) => {
            free_ty_vars(ctx, a, acc);
            free_ty_vars(ctx, b, acc);
        }
        Type::Tuple(ts) => {
            for ty in ts {
                free_ty_vars(ctx, &ty, acc);
            }
        }
        Type::Struct(_, _) => todo!(),
    }
}

fn free_env_vars(ctx: &mut TypeContext, env: &Env) -> HashSet<TypeVarId> {
    let mut acc = HashSet::new();
    for scheme in env.map.values() {
        free_ty_vars(ctx, &scheme.ty, &mut acc);
        for v in &scheme.vars {
            acc.remove(v);
        }
    }
    acc
}

// ===== Instantiate / Generalize =====
fn instantiate(ctx: &mut TypeContext, sch: &Scheme) -> Type {
    let mut subst: HashMap<TypeVarId, TypeVarId> = HashMap::new();
    for &v in &sch.vars {
        subst.insert(v, ctx.fresh_var());
    }
    substitute(ctx, &sch.ty, &subst)
}

fn substitute(ctx: &mut TypeContext, t: &Type, subst: &HashMap<TypeVarId, TypeVarId>) -> Type {
    match ctx.repr(t) {
        Type::Var(v) => {
            if let Some(nv) = subst.get(&v) {
                Type::Var(*nv)
            } else {
                Type::Var(v)
            }
        }
        Type::Fun(ref a, ref b) => {
            Type::fun(substitute(ctx, a, subst), substitute(ctx, b, subst))
        }
        Type::Con(c) => {
            Type::con(c)
        }
        Type::App(ref a, ref b) => {
            Type::app(substitute(ctx, a, subst), substitute(ctx, b, subst))
        }
        Type::Tuple(ts) => {
            Type::Tuple(ts.iter().map(|t| substitute(ctx, t, subst)).collect())
        }
        Type::Struct(_, _) => todo!(),
    }
}

pub fn generalize(ctx: &mut TypeContext, env: &Env, ty: &Type) -> Scheme {
    let mut fty = HashSet::new();
    free_ty_vars(ctx, ty, &mut fty);
    let fenv = free_env_vars(ctx, env);
    let mut vars: Vec<TypeVarId> = fty.difference(&fenv).cloned().collect();
    vars.sort_by_key(|v| v.0);
    Scheme::poly(vars, ctx.repr(ty))
}

// ===== Inference (Algorithm J core) =====
pub fn infer(ctx: &mut TypeContext, env: &mut Env, expr: &Expr) -> Result<Type, TypeError> {
    match expr {
        Expr::Var(x) => {
            let sch = env.map.get(x).ok_or_else(|| TypeError::UnboundVariable(format!("{}",x)))?;
            Ok(instantiate(ctx, sch))
        }
        Expr::Abs(x, body) => {
            // introduce parameter with fresh monomorphic type variable
            let tv = Type::Var(ctx.fresh_var());
            // extend env temporarily
            let saved = env.clone();
            env.map.insert(x.clone(), Scheme::mono(tv.clone()));
            let tbody = infer(ctx, env, body)?;
            *env = saved;
            Ok(Type::fun(tv, tbody))
        }
        Expr::App(f, a) => {
            let tf = infer(ctx, env, f)?;
            let ta = infer(ctx, env, a)?;
            let tr = Type::Var(ctx.fresh_var()); // result type variable
            ctx.unify(&tf, &Type::fun(ta, tr.clone()))?;
            Ok(tr)
        }
        Expr::Let(pat, e1, e2) => {
            let t1 = infer(ctx, env, e1)?;
            let t_pat = ctx.fresh_type_for_pattern(&pat); // 例: (α, β)
            ctx.unify(&t1, &t_pat)?;
            let mut env2 = env.clone();               // 新しいスコープ
            ctx.match_pattern(&mut env2, &pat, &t_pat)?;
            infer(ctx, &mut env2, e2)
        }
        Expr::LetRec(pat, e1, e2) => {
            match pat {
                Pat::Var(x) => {
                    let tv = Type::Var(ctx.fresh_var());
                    let mut env2 = env.clone(); // 新しいスコープ
                    env2.map.insert(x.clone(), Scheme::mono(tv.clone())); // x は e1 にも見える
                    let t1 = infer(ctx, &mut env2, e1)?;
                    ctx.unify(&tv, &t1)?;
                    infer(ctx, &mut env2, e2)
                }
                _ => Err(TypeError::LetRecPatternNotSupported(pat.clone()))
            }
        }
        // Expr::LetRec(x, e1, e2) => {
        //     // 1. fresh type variable
        //     let tv = Type::Var(ctx.fresh_var());
        //     // 2. 環境に仮の型を追加
        //     env.map.insert(x.clone(), Scheme { vars: vec![], ty: tv.clone() });
        //     // 3. e1 の型を推論
        //     let t1 = infer(ctx, env, e1)?;
        //     // 4. unify で仮の型と一致させる
        //     ctx.unify(&tv, &t1)?;
        //     // 5. 一般化して環境に登録し直す
        //     let sch = generalize(ctx, env, &t1);
        //     env.map.insert(x.clone(), sch);
        //     // 6. e2 の型を推論
        //     infer(ctx, env, e2)
        // }
        Expr::If(cond, then_e, else_e) => {
            let t_cond = infer(ctx, env, cond)?;
            // ctx.unify(&t_cond, &Type::Bool)?;
            ctx.unify(&t_cond, &Type::Con("Bool".into()))?;

            let t_then = infer(ctx, env, then_e)?;
            let t_else = infer(ctx, env, else_e)?;
            ctx.unify(&t_then, &t_else)?;
            Ok(t_then)
        }

        Expr::Lit(Lit::Unit) => Ok(Type::Con("()".into())),
        Expr::Lit(Lit::Bool(_)) => Ok(Type::Con("Bool".into())),
        Expr::Lit(Lit::Int(_)) => Ok(Type::Con("Int".into())),

        Expr::Tuple(es) => {
            let mut tys = Vec::new();
            for e in es {
                let ty = infer(ctx, env, e)?;
                tys.push(ty);
            }
            Ok(Type::Tuple(tys))
        }

        Expr::Struct(_, _) => todo!(),
    }
}

// ------------------------
pub fn initial_type_env() -> TypeEnv {
    let mut tenv = TypeEnv { kinds: HashMap::new() };

    tenv.kinds.insert("Int".into(), Kind::Star);
    tenv.kinds.insert("Bool".into(), Kind::Star);
    tenv.kinds.insert("List".into(), Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star)));
    tenv.kinds.insert("Option".into(), Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star)));

    tenv
}

pub fn initial_env(ctx: &mut TypeContext) -> Env {
    let mut env = Env::default();

    // None : ∀a. Option a
    let a = ctx.fresh_var();
    env.map.insert(
        "None".into(),
        Scheme {
            vars: vec![a],
            ty: Type::app(
                Type::con("Option"),
                Type::var(a),
            ),
        },
    );

    // Some : ∀a. a -> Option a
    let a = ctx.fresh_var();
    env.map.insert(
        "Some".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(
                Type::var(a),
                Type::app(
                    Type::con("Option"),
                    Type::var(a),
                ),
            ),
        },
    );

    // Nil : ∀a. List a
    let a = ctx.fresh_var();
    env.map.insert(
        "Nil".into(),
        Scheme {
            vars: vec![a],
            ty: Type::app(Type::con("List"), Type::var(a)),
        },
    );

    // Cons : ∀a. a -> List a -> List a
    let a = ctx.fresh_var();
    env.map.insert(
        "Cons".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(
                Type::var(a),
                Type::fun(
                    Type::app(Type::con("List"), Type::var(a)),
                    Type::app(Type::con("List"), Type::var(a)),
                ),
            ),
        },
    );

    // map : ∀a b. (a -> b) -> List a -> List b
    let a = ctx.fresh_var();
    let b = ctx.fresh_var();

    env.map.insert(
        "map".into(),
        Scheme {
            vars: vec![a, b],
            ty: Type::fun(
                Type::fun(Type::var(a), Type::var(b)), // (a -> b)
                Type::fun(
                    Type::app(Type::con("List"), Type::var(a)), // List a
                    Type::app(Type::con("List"), Type::var(b)), // List b
                ),
            ),
        },
    );

    // プリミティブ演算子 (+, -, *, ==) も必要なら追加
    let a = ctx.fresh_var();
    env.map.insert(
        "+".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::var(a))),
        },
    );

    let a = ctx.fresh_var();
    env.map.insert(
        "-".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::var(a))),
        },
    );

    let a = ctx.fresh_var();
    env.map.insert(
        "*".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::var(a))),
        },
    );

    let a = ctx.fresh_var();
    env.map.insert(
        "==".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::con("Bool"))),
        },
    );

    env
}
