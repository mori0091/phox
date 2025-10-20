use std::collections::{HashMap, HashSet};

use crate::syntax::ast::{Expr, Lit, Pat, Stmt, Item};
use super::{Kind, Type, TypeVarId, Scheme};

// ===== Type error =====
#[derive(Debug)]
pub enum TypeError {
    UnboundVariable(String),
    Mismatch(Type, Type),
    RecursiveType,

    ExpectedTuple(Type),
    IndexOutOfBounds(usize, Type),
    TupleLengthMismatch(usize, usize),

    UnknownConstructor(String),
    ConstructorArityMismatch(String, usize, Type),

    EmptyMatch,
    UnsupportedPattern(Pat),
    LetRecPatternNotSupported(Pat),

    ExpectedRecord(Type),
    UnknownField(String, Type),
}

// ===== Kind Environment =====
// maps name of type constructor to Kind
pub type KindEnv = HashMap<String, Kind>;

// ===== Type Environment =====
// maps name of variable to type scheme
pub type TypeEnv = HashMap<String, Scheme>;

// ===== Type context: union-find + binding =====
pub struct TypeContext {
    parent: Vec<TypeVarId>,    // union-find parent pointers
    binding: Vec<Option<Type>>, // representative binding (Some if bound to a type)
}

impl TypeContext {
    pub fn new() -> Self {
        Self { parent: Vec::new(), binding: Vec::new() }
    }

    pub fn fresh_type_var_id(&mut self) -> TypeVarId {
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
            Type::Fun(a, b) | Type::App(a, b) => {
                self.occurs_in(tv, a) || self.occurs_in(tv, b)
            }
            Type::Tuple(ts) => {
                ts.iter().any(|t| self.occurs_in(tv, t))
            }
            Type::Record(fields) => {
                fields.iter().any(|(_, t)| self.occurs_in(tv, t))
            }
            Type::Con(_name) => false,
        }
    }
}

impl TypeContext {
    // Normalize a type by chasing bindings and compressing vars
    pub fn repr(&mut self, ty: &Type) -> Type {
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

            Type::Tuple(ts) => {
                Type::Tuple(
                    ts.iter()
                      .map(|t| self.repr(t)).collect()
                )
            }

            Type::Record(fields) => {
                Type::Record(
                    fields.iter()
                          .map(|(field, t)| (field.clone(), self.repr(t)))
                          .collect()
                )
            }
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

            // フィールド名で照合、並び順は問わない
            (Type::Record(fields1), Type::Record(fields2)) => {
                // まずフィールド数が一致しているか確認
                if fields1.len() != fields2.len() {
                    return Err(TypeError::Mismatch(
                        Type::Record(fields1.clone()),
                        Type::Record(fields2.clone()),
                    ));
                }

                // 名前で対応付けて unify
                for (fname, ty1) in fields1 {
                    match fields2.iter().find(|(n, _)| n == fname) {
                        Some((_, ty2)) => self.unify(ty1, ty2)?,
                        None => {
                            return Err(TypeError::UnknownField(
                                fname.clone(),
                                Type::Record(fields2.clone())
                            ));
                        }
                    }
                }

                Ok(())
            }

            _ => Err(TypeError::Mismatch(a, b)),
        }
    }
}

impl TypeContext {
    fn fresh_type_for_pattern(&mut self, pat: &Pat) -> Type {
        match pat {
            Pat::Var(_) | Pat::Wildcard => Type::Var(self.fresh_type_var_id()),
            Pat::Lit(lit) => match lit {
                Lit::Unit => Type::con("()"),
                Lit::Bool(_) => Type::con("Bool"),
                Lit::Int(_) => Type::con("Int"),
            },
            Pat::Tuple(ps) => {
                let ts = ps.iter().map(|p| self.fresh_type_for_pattern(p)).collect();
                Type::Tuple(ts)
            }
            Pat::Con(_, _) => {
                Type::Var(self.fresh_type_var_id()) // パターン全体の型は未知とする
            }
            Pat::Record(fields) => {
                let tys = fields.iter()
                                .map(|(name, _p)| (name.clone(), Type::Var(self.fresh_type_var_id())))
                                .collect();
                Type::Record(tys)
            }
        }
    }
}

impl TypeContext {
    fn match_pattern(
        &mut self,
        env: &mut TypeEnv,
        pat: &Pat,
        ty: &Type,
        outer_env: &TypeEnv,
        generalize_bindings: bool,
    ) -> Result<(), TypeError> {
        match pat {
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

            Pat::Var(x) => {
                let sch = if generalize_bindings {
                    generalize(self, outer_env, ty)   // let の場合
                } else {
                    Scheme::mono(self.repr(ty))       // Abs や match の場合は単相
                };
                env.insert(x.clone(), sch);
                Ok(())
            }

            Pat::Con(name, args) => {
                let scheme = env.get(name).ok_or(TypeError::UnknownConstructor(name.clone()))?;
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
                    self.match_pattern(env, p, t, outer_env, generalize_bindings)?;
                }

                Ok(())
            }

            Pat::Tuple(ps) => {
                match ty {
                    Type::Tuple(ts) => {
                        if ps.len() != ts.len() {
                            return Err(TypeError::TupleLengthMismatch(ps.len(), ts.len()));
                        }
                        for (p, t) in ps.iter().zip(ts.iter()) {
                            self.match_pattern(env, p, t, outer_env, generalize_bindings)?;
                        }
                        Ok(())
                    }
                    _ => Err(TypeError::ExpectedTuple(ty.clone())),
                }
            }

            Pat::Record(fields) => {
                let ty = self.repr(ty);
                if let Type::Record(ref tys) = ty {
                    for (fname, p) in fields {
                        let ft = tys.iter()
                                    .find(|(n, _)| n == fname)
                                    .ok_or_else(|| TypeError::UnknownField(fname.clone(), ty.clone()))?
                                    .1.clone();
                        self.match_pattern(env, p, &ft, outer_env, generalize_bindings)?;
                    }
                    Ok(())
                } else {
                    Err(TypeError::Mismatch(ty, Type::Record(vec![])))
                }
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
        Type::Record(ref fields) => {
            for (_, field_ty) in fields {
                free_ty_vars(ctx, field_ty, acc);
            }
        }
    }
}

fn free_env_vars(ctx: &mut TypeContext, env: &TypeEnv) -> HashSet<TypeVarId> {
    let mut acc = HashSet::new();
    for scheme in env.values() {
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
        subst.insert(v, ctx.fresh_type_var_id());
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
        Type::Record(fields) => {
            Type::Record(
                fields.iter()
                      .map(|(f, t)| (f.clone(), substitute(ctx, t, subst)))
                      .collect()
            )
        }
    }
}

pub fn generalize(ctx: &mut TypeContext, env: &TypeEnv, ty: &Type) -> Scheme {
    let mut fty = HashSet::new();
    free_ty_vars(ctx, ty, &mut fty);
    let fenv = free_env_vars(ctx, env);
    let mut vars: Vec<TypeVarId> = fty.difference(&fenv).cloned().collect();
    vars.sort_by_key(|v| v.0);
    Scheme::poly(vars, ctx.repr(ty))
}

// ===== Inference (Algorithm J core) =====
pub fn infer_item(ctx: &mut TypeContext, tenv: &mut TypeEnv, item: &Item) -> Result<Type, TypeError> {
    match item {
        Item::Stmt(stmt) => {
            infer_stmt(ctx, tenv, stmt)
        }
        Item::Expr(expr) => {
            infer_expr(ctx, tenv, expr)
        }
        _ => Ok(Type::con("()"))
    }
}

pub fn infer_stmt(ctx: &mut TypeContext, tenv: &mut TypeEnv, stmt: &Stmt) -> Result<Type, TypeError> {
    match stmt {
        Stmt::Let(pat, expr) => {
            let t_expr = infer_expr(ctx, tenv, expr)?;
            let t_pat = ctx.fresh_type_for_pattern(pat);
            ctx.unify(&t_expr, &t_pat)?;
            let ref_tenv = tenv.clone(); // snapshot for reference
            ctx.match_pattern(tenv, pat, &t_pat, &ref_tenv, true)?;
            Ok(Type::con("()"))
        }
        Stmt::LetRec(pat, expr) => {
            match pat {
                Pat::Var(x) => {
                    let tv = Type::Var(ctx.fresh_type_var_id());
                    tenv.insert(x.clone(), Scheme::mono(tv.clone()));
                    let t_expr = infer_expr(ctx, tenv, expr)?;
                    ctx.unify(&tv, &t_expr)?;
                    let sch = generalize(ctx, tenv, &tv);
                    tenv.insert(x.clone(), sch);
                    Ok(Type::con("()"))
                }
                _ => Err(TypeError::LetRecPatternNotSupported(pat.clone())),
            }
        }
    }
}

pub fn infer_expr(ctx: &mut TypeContext, tenv: &mut TypeEnv, expr: &Expr) -> Result<Type, TypeError> {
    match expr {
        Expr::Var(x) => {
            let sch = tenv.get(x).ok_or_else(|| TypeError::UnboundVariable(format!("{}",x)))?;
            Ok(instantiate(ctx, sch))
        }
        Expr::Abs(pat, body) => {
            // パターンに対応する型を生成
            let t_pat = ctx.fresh_type_for_pattern(pat);

            // 環境を拡張
            let mut env2 = tenv.clone();
            // ctx.match_pattern(&mut env2, pat, &t_pat, tenv)?;
            ctx.match_pattern(&mut env2, pat, &t_pat, tenv, false)?;

            // 本体を推論
            let t_body = infer_expr(ctx, &mut env2, body)?;

            // 関数型を返す
            Ok(Type::fun(t_pat, t_body))
        }

        Expr::App(f, a) => {
            let tf = infer_expr(ctx, tenv, f)?;
            let ta = infer_expr(ctx, tenv, a)?;
            let tr = Type::Var(ctx.fresh_type_var_id()); // result type variable
            ctx.unify(&tf, &Type::fun(ta, tr.clone()))?;
            Ok(tr)
        }
        Expr::Block(items) => {
            let mut tenv2 = tenv.clone(); // 新しいスコープ
            let mut last_ty = Type::con("()");
            for item in items {
                last_ty = infer_item(ctx, &mut tenv2, item)?;
            }
            Ok(last_ty)
        }
        Expr::If(cond, then_e, else_e) => {
            let t_cond = infer_expr(ctx, tenv, cond)?;
            ctx.unify(&t_cond, &Type::Con("Bool".into()))?;

            let t_then = infer_expr(ctx, tenv, then_e)?;
            let t_else = infer_expr(ctx, tenv, else_e)?;
            ctx.unify(&t_then, &t_else)?;
            Ok(t_then)
        }

        Expr::Match(scrutinee, arms) => {
            // 判別対象式の型を推論
            let t_scrut = infer_expr(ctx, tenv, scrutinee)?;

            // 各アームの式型を集める
            let mut result_types = vec![];

            for (pat, body) in arms {
                // パターンに対応する型を生成（型変数を含む構造）
                let t_pat = ctx.fresh_type_for_pattern(pat);

                // scrutinee の型とパターン型を unify
                ctx.unify(&t_scrut, &t_pat)?;

                // 束縛環境を構築
                let mut env2 = tenv.clone();
                ctx.match_pattern(&mut env2, pat, &t_pat, tenv, false)?;

                // アーム本体の型を推論
                let t_body = infer_expr(ctx, &mut env2, body)?;
                result_types.push(t_body);
            }

            let mut result_types = result_types.into_iter();
            let t_result = result_types.next().ok_or(TypeError::EmptyMatch)?;
            for ty in result_types {
                ctx.unify(&t_result, &ty)?;
            }

            Ok(t_result)
        }

        Expr::Lit(Lit::Unit) => Ok(Type::Con("()".into())),
        Expr::Lit(Lit::Bool(_)) => Ok(Type::Con("Bool".into())),
        Expr::Lit(Lit::Int(_)) => Ok(Type::Con("Int".into())),

        Expr::Tuple(es) => {
            let mut tys = Vec::new();
            for e in es {
                let ty = infer_expr(ctx, tenv, e)?;
                tys.push(ty);
            }
            Ok(Type::Tuple(tys))
        }

        Expr::Record(fields) => {
            // 各フィールドの型を推論
            let mut typed_fields = Vec::with_capacity(fields.len());
            for (fname, fexpr) in fields {
                let t_field = infer_expr(ctx, tenv, fexpr)?;
                typed_fields.push((fname.clone(), t_field));
            }
            Ok(Type::Record(typed_fields))
        }

        Expr::FieldAccess(base, field) => {
            let t_base = infer_expr(ctx, tenv, base)?;
            match t_base {
                Type::Record(fields) => {
                    if let Some((_, ty)) = fields.iter().find(|(fname, _)| fname == field) {
                        Ok(ty.clone())
                    } else {
                        Err(TypeError::UnknownField(field.clone(), Type::Record(fields)))
                    }
                }
                other => {
                    if let Some(con) = is_tycon(&other) {
                        let pat = Pat::con(con, vec![Pat::var("r")]);
                        let p = base.clone();
                        let expr = Expr::Block(vec![
                            Item::Stmt(Stmt::Let(pat, p)),
                            Item::Expr(Expr::field_access(Expr::var("r"), field.clone()))
                        ]);
                        infer_expr(ctx, tenv, &expr).map_err(|_| TypeError::ExpectedRecord(other))
                    }
                    else {
                        Err(TypeError::ExpectedRecord(other))
                    }
                }
            }
        }

        Expr::TupleAccess(base, index) => {
            let t_base = infer_expr(ctx, tenv, base)?;
            match t_base {
                Type::Tuple(elems) => {
                    if *index < elems.len() {
                        Ok(elems[*index].clone())
                    } else {
                        Err(TypeError::IndexOutOfBounds(*index, Type::Tuple(elems)))
                    }
                }
                other => {
                    if let Some(con) = is_tycon(&other) {
                        let pat = Pat::con(con, vec![Pat::var("t")]);
                        let p = base.clone();
                        let expr = Expr::Block(vec![
                            Item::Stmt(Stmt::Let(pat, p)),
                            Item::Expr(Expr::tuple_access(Expr::var("t"), *index))
                        ]);
                        infer_expr(ctx, tenv, &expr).map_err(|_| TypeError::ExpectedTuple(other))
                    }
                    else {
                        Err(TypeError::ExpectedTuple(other))
                    }
                }
            }
        }
    }
}

fn is_tycon(mut t: &Type) -> Option<String> {
    while let Type::App(a, _) = t {
        t = a;
    }
    match t {
        Type::Con(con) => Some(con.clone()),
        _ => None,
    }
}

// ------------------------
pub fn initial_kind_env() -> KindEnv {
    let mut env = KindEnv::new();

    env.insert("Int".into(), Kind::Star);
    env.insert("Bool".into(), Kind::Star);
    env.insert("List".into(), Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star)));
    env.insert("Option".into(), Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star)));

    env
}

pub fn initial_type_env(ctx: &mut TypeContext) -> TypeEnv {
    let mut env = TypeEnv::new();

    // None : ∀a. Option a
    let a = ctx.fresh_type_var_id();
    env.insert(
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
    let a = ctx.fresh_type_var_id();
    env.insert(
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
    let a = ctx.fresh_type_var_id();
    env.insert(
        "Nil".into(),
        Scheme {
            vars: vec![a],
            ty: Type::app(Type::con("List"), Type::var(a)),
        },
    );

    // Cons : ∀a. a -> List a -> List a
    let a = ctx.fresh_type_var_id();
    env.insert(
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
    let a = ctx.fresh_type_var_id();
    let b = ctx.fresh_type_var_id();

    env.insert(
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

    let a = ctx.fresh_type_var_id();
    env.insert(
        "==".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::con("Bool"))),
        },
    );

    let a = ctx.fresh_type_var_id();
    env.insert(
        "!=".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::con("Bool"))),
        },
    );

    let a = ctx.fresh_type_var_id();
    env.insert(
        "<".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::con("Bool"))),
        },
    );


    let a = ctx.fresh_type_var_id();
    env.insert(
        "<=".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::con("Bool"))),
        },
    );

    let a = ctx.fresh_type_var_id();
    env.insert(
        ">".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::con("Bool"))),
        },
    );

    let a = ctx.fresh_type_var_id();
    env.insert(
        ">=".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::con("Bool"))),
        },
    );

    let a = ctx.fresh_type_var_id();
    env.insert(
        "+".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::var(a))),
        },
    );

    let a = ctx.fresh_type_var_id();
    env.insert(
        "-".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::var(a))),
        },
    );

    let a = ctx.fresh_type_var_id();
    env.insert(
        "*".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::var(a))),
        },
    );

    let a = ctx.fresh_type_var_id();
    env.insert(
        "/".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::var(a))),
        },
    );

    let a = ctx.fresh_type_var_id();
    env.insert(
        "neg".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::var(a)),
        },
    );

    let a = ctx.fresh_type_var_id();
    env.insert(
        "not".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::var(a)),
        },
    );

    env
}
