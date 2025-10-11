use std::collections::{HashMap, HashSet};

use crate::syntax::ast::{Expr, Lit, Pat};
use super::{Kind, Type, TypeVarId, Scheme};

type Env<T> = HashMap<String, T>;

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
    EmptyMatch,
    UnknownField(String, String),
    ExpectedStruct(String, Type),
}

// ===== Kind Environment =====
// maps name of type constructor to Kind
type KindEnv = Env<Kind>;

// ===== Type Environment =====
// maps name of variable to type scheme
type TypeEnv = Env<Scheme>;

// ===== Type context: union-find + binding =====
pub struct TypeContext {
    parent: Vec<TypeVarId>,    // union-find parent pointers
    binding: Vec<Option<Type>>, // representative binding (Some if bound to a type)
}

impl TypeContext {
    pub fn new() -> Self {
        Self { parent: Vec::new(), binding: Vec::new() }
    }

    fn fresh_var(&mut self) -> TypeVarId {
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
            Type::Struct(_name, fields) => {
                fields.iter().any(|(_, t)| self.occurs_in(tv, t))
            }
            Type::Con(_name) => false,
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

            Type::Tuple(ts) => {
                Type::Tuple(
                    ts.iter()
                      .map(|t| self.repr(t)).collect()
                )
            }

            Type::Struct(name, fields) => {
                Type::Struct(
                    name.clone(),
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

            // // フィールドの並び順も含め全て一致しないとエラー
            // (Type::Struct(s1, fs1), Type::Struct(s2, fs2)) if s1 == s2 => {
            //     for ((f1, t1), (f2, t2)) in fs1.iter().zip(fs2.iter()) {
            //         if f1 != f2 {
            //             return Err(TypeError::Mismatch(a, b));
            //         }
            //         self.unify(t1, t2)?;
            //     }
            //     Ok(())
            // }

            // フィールド名で照合、並び順は問わない
            (Type::Struct(name1, fields1), Type::Struct(name2, fields2)) if name1 == name2 => {
                // まずフィールド数が一致しているか確認
                if fields1.len() != fields2.len() {
                    return Err(TypeError::Mismatch(
                        Type::Struct(name1.clone(), fields1.clone()),
                        Type::Struct(name2.clone(), fields2.clone()),
                    ));
                }

                // 名前で対応付けて unify
                for (fname, ty1) in fields1 {
                    match fields2.iter().find(|(n, _)| n == fname) {
                        Some((_, ty2)) => self.unify(ty1, ty2)?,
                        None => {
                            return Err(TypeError::UnknownField(name2.clone(), fname.clone()));
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
            Pat::Struct(name, fields) => {
                let field_types
                    = fields.iter()
                            .map(|(_, _)| self.fresh_var())
                            .collect::<Vec<_>>();

                let typed_fields
                    = fields.iter()
                            .zip(field_types.iter())
                            .map(|((k, _), t)| (k.clone(), Type::Var(*t)))
                            .collect();

                Type::Struct(name.clone(), typed_fields)
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
                let sch = generalize(self, outer_env, ty);
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
                    self.match_pattern(env, p, t, outer_env)?;
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
                            self.match_pattern(env, p, t, outer_env)?;
                        }
                        Ok(())
                    }
                    _ => Err(TypeError::ExpectedTuple(ty.clone())),
                }
            }

            Pat::Struct(name, fields) => {
                match ty {
                    Type::Struct(ty_name, ty_fields) if ty_name == name => {
                        for (pname, ppat) in fields {
                            match ty_fields.iter().find(|(k, _)| k == pname) {
                                Some((_, t_field)) => {
                                    self.match_pattern(env, ppat, t_field, outer_env)?;
                                }
                                None => {
                                    return Err(TypeError::UnknownField(name.clone(), pname.clone()));
                                }
                            }
                        }
                        Ok(())
                    }
                    _ => Err(TypeError::ExpectedStruct(name.clone(), ty.clone())),
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
        Type::Struct(_, ref fields) => {
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

pub fn generalize(ctx: &mut TypeContext, env: &TypeEnv, ty: &Type) -> Scheme {
    let mut fty = HashSet::new();
    free_ty_vars(ctx, ty, &mut fty);
    let fenv = free_env_vars(ctx, env);
    let mut vars: Vec<TypeVarId> = fty.difference(&fenv).cloned().collect();
    vars.sort_by_key(|v| v.0);
    Scheme::poly(vars, ctx.repr(ty))
}

// ===== Inference (Algorithm J core) =====
pub fn infer(ctx: &mut TypeContext, env: &mut TypeEnv, expr: &Expr) -> Result<Type, TypeError> {
    match expr {
        Expr::Var(x) => {
            let sch = env.get(x).ok_or_else(|| TypeError::UnboundVariable(format!("{}",x)))?;
            Ok(instantiate(ctx, sch))
        }
        Expr::Abs(x, body) => {
            // introduce parameter with fresh monomorphic type variable
            let tv = Type::Var(ctx.fresh_var());
            // extend env temporarily
            let saved = env.clone();
            env.insert(x.clone(), Scheme::mono(tv.clone()));
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
            ctx.match_pattern(&mut env2, &pat, &t_pat, env)?;
            infer(ctx, &mut env2, e2)
        }
        Expr::LetRec(pat, e1, e2) => {
            match pat {
                Pat::Var(x) => {
                    let tv = Type::Var(ctx.fresh_var());
                    let mut env2 = env.clone();
                    // ★ 先に仮の型を入れてから e1 を推論
                    env2.insert(x.clone(), Scheme::mono(tv.clone()));

                    let t1 = infer(ctx, &mut env2, e1)?;
                    ctx.unify(&tv, &t1)?;

                    // ★ 一般化して確定
                    // (外側の`env`環境下を使って一般化すること！)
                    let sch = generalize(ctx, env, &tv);
                    // ここの `insert` は挿入ではなく「差し替え」
                    env2.insert(x.clone(), sch);

                    infer(ctx, &mut env2, e2)
                }
                _ => Err(TypeError::LetRecPatternNotSupported(pat.clone())),
            }
        }
        Expr::If(cond, then_e, else_e) => {
            let t_cond = infer(ctx, env, cond)?;
            ctx.unify(&t_cond, &Type::Con("Bool".into()))?;

            let t_then = infer(ctx, env, then_e)?;
            let t_else = infer(ctx, env, else_e)?;
            ctx.unify(&t_then, &t_else)?;
            Ok(t_then)
        }

        Expr::Match(scrutinee, arms) => {
            // 判別対象式の型を推論
            let t_scrut = infer(ctx, env, scrutinee)?;

            // 各アームの式型を集める
            let mut result_types = vec![];

            for (pat, body) in arms {
                // パターンに対応する型を生成（型変数を含む構造）
                let t_pat = ctx.fresh_type_for_pattern(pat);

                // scrutinee の型とパターン型を unify
                ctx.unify(&t_scrut, &t_pat)?;

                // 束縛環境を構築
                let mut env2 = env.clone();
                ctx.match_pattern(&mut env2, pat, &t_pat, env)?;

                // アーム本体の型を推論
                let t_body = infer(ctx, &mut env2, body)?;
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
                let ty = infer(ctx, env, e)?;
                tys.push(ty);
            }
            Ok(Type::Tuple(tys))
        }

        Expr::Struct(name, fields) => {
            // 各フィールドの型を推論
            let mut typed_fields = Vec::with_capacity(fields.len());
            for (fname, fexpr) in fields {
                let t_field = infer(ctx, env, fexpr)?;
                typed_fields.push((fname.clone(), t_field));
            }

            // // 既知の構造体型が env にあるなら照合
            // if let Some(scheme) = env.get_type_of_struct(name) {
            //     let declared = instantiate(ctx, scheme); // e.g., Type::Struct(name, [(x, α), (y, β)])
            //     // declared と推論結果をフィールド名で突き合わせて unify
            //     match (declared, Type::Struct(name.clone(), typed_fields.clone())) {
            //         (Type::Struct(_, decl_fields), Type::Struct(_, inf_fields)) => {
            //             for (fname, t_inf) in &inf_fields {
            //                 if let Some((_, t_decl)) = decl_fields.iter().find(|(n, _)| n == fname) {
            //                     ctx.unify(t_inf, t_decl)?;
            //                 } else {
            //                     return Err(TypeError::UnknownField(name.clone(), fname.clone()));
            //                 }
            //             }
            //         }
            //         other => return Err(TypeError::Mismatch(other.0, other.1)),
            //     }
            // }

            // 構造体型を構築して返す
            Ok(Type::Struct(name.clone(), typed_fields))
        }
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
    let mut env = TypeEnv::default();

    // None : ∀a. Option a
    let a = ctx.fresh_var();
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
    let a = ctx.fresh_var();
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
    let a = ctx.fresh_var();
    env.insert(
        "Nil".into(),
        Scheme {
            vars: vec![a],
            ty: Type::app(Type::con("List"), Type::var(a)),
        },
    );

    // Cons : ∀a. a -> List a -> List a
    let a = ctx.fresh_var();
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
    let a = ctx.fresh_var();
    let b = ctx.fresh_var();

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

    // プリミティブ演算子 (+, -, *, ==) も必要なら追加
    let a = ctx.fresh_var();
    env.insert(
        "+".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::var(a))),
        },
    );

    let a = ctx.fresh_var();
    env.insert(
        "-".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::var(a))),
        },
    );

    let a = ctx.fresh_var();
    env.insert(
        "*".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::var(a))),
        },
    );

    let a = ctx.fresh_var();
    env.insert(
        "/".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::var(a))),
        },
    );

    let a = ctx.fresh_var();
    env.insert(
        "==".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::con("Bool"))),
        },
    );

    let a = ctx.fresh_var();
    env.insert(
        "!=".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::con("Bool"))),
        },
    );

    let a = ctx.fresh_var();
    env.insert(
        "<".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::con("Bool"))),
        },
    );


    let a = ctx.fresh_var();
    env.insert(
        "<=".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::con("Bool"))),
        },
    );

    let a = ctx.fresh_var();
    env.insert(
        ">".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::con("Bool"))),
        },
    );

    let a = ctx.fresh_var();
    env.insert(
        ">=".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::con("Bool"))),
        },
    );

    env
}
