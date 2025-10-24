use std::collections::{HashMap, HashSet};

use crate::syntax::ast::{Expr, ExprBody, Lit, Pat, Stmt, Item};
use super::{Kind, Type, TypeVarId, Constraint, Scheme};
use super::TypeError;

// ===== Kind Environment =====
// maps name of type constructor to Kind
pub type KindEnv = HashMap<String, Kind>;

// ===== Type Environment =====
// maps name of variable to type scheme.
// ex.
// {
//   "id": ∀ a. a -> a,
// }
pub type TypeEnv = HashMap<String, Scheme>;

// ===== Type Environment for trait member =====
// maps name of trait member to set of type schemes.
// ex.
// {
//   "f": {
//     ∀ a. Foo a => a -> a,
//     ∀ a. Bar a => a -> a,
//   },
// }
pub type TraitMemberEnv = HashMap<String, HashSet<Scheme>>;

// ===== Impl Environment =====
// maps instance of trait to dictionary of its implementations.
// ex.
// {
//   (Eq Int): {
//     "==": primitive_eq_int,
//     "!=": primitive_neq_int,
//   },
// }
pub type ImplEnv = HashMap<Constraint, HashMap<String, Expr>>;

// ===== Infer Context =====
#[derive(Clone)]
pub struct InferCtx {
    pub kind_env: KindEnv,            // 型コンストラクタの kind 情報 (ex. List: * -> *)
    pub type_env: TypeEnv,            // スコープ内の識別子の型スキーム (ex. `==`: ∀ a. a -> a -> Bool)
    pub trait_member_env: TraitMemberEnv, // traitメンバの型スキーム集合 (ex. "f": { ∀ a. Foo a => a -> a, ∀ a. Bar a => a -> a })
    pub impl_member_env: TraitMemberEnv, // implメンバの型スキーム集合 (ex. "f": { ∀ Int. Foo Int => Int -> Int, ∀ Bool. Foo Bool => Bool -> Bool })
    // pub obligations: Vec<Constraint>, // 推論中に発生した未解決の制約(ex. Eq α)
}

impl InferCtx {
    pub fn new() -> Self {
        Self {
            kind_env: KindEnv::new(),
            type_env: TypeEnv::new(),
            trait_member_env: TraitMemberEnv::new(),
            impl_member_env: TraitMemberEnv::new(),
            // obligations: vec![],
        }
    }

    pub fn initial(ctx: &mut TypeContext) -> Self {
        let kind_env = initial_kind_env();
        let type_env = initial_type_env(ctx);
        let trait_member_env = TraitMemberEnv::new();
        let impl_member_env = TraitMemberEnv::new();
        // let obligations = vec![];
        InferCtx {
            kind_env,
            type_env,
            trait_member_env,
            impl_member_env,
            // obligations,
        }
    }
}

// ===== Type context: union-find + binding =====
#[derive(Clone)]
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
            Type::Overloaded(_name, _cands) => {
                todo!()
            }
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
            Type::Overloaded(name, cands) => {
                let mut new_cands = vec![];
                for sch in cands.iter() {
                    let vars = sch.vars.iter().map(|v| self.find(*v)).collect();
                    let constraints = sch.constraints.iter().map(|c| {
                        let params = c.params.iter().map(|t| self.repr(t)).collect();
                        Constraint { name: c.name.clone(), params }
                    }).collect();
                    let ty = self.repr(&sch.ty);
                    new_cands.push(Scheme::new(vars, constraints, ty));
                }
                Type::Overloaded(name.clone(), new_cands)
            }
        }
    }
}

impl TypeContext {
    pub fn unify(&mut self, a: &Type, b: &Type) -> Result<(), TypeError> {
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

            (Type::Overloaded(_, _), Type::Overloaded(_, _)) => {
                todo!()
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
        icx: &mut InferCtx,
        pat: &Pat,
        ty: &Type,
        outer_icx: &InferCtx,
        generalize_bindings: bool,
    ) -> Result<(), TypeError> {
        match pat {
            Pat::Wildcard => Ok(()), // 束縛なし

            Pat::Lit(lit) => {
                let expected = match lit {
                    Lit::Unit => Type::Con("()".to_string()),
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
                    generalize(self, &outer_icx, ty)   // let の場合
                } else {
                    Scheme::mono(self.repr(ty))       // Abs や match の場合は単相
                };
                icx.type_env.insert(x.clone(), sch);
                Ok(())
            }

            Pat::Con(name, args) => {
                let scheme = icx.type_env.get(name).ok_or(TypeError::UnknownConstructor(name.clone()))?;
                let (_constraints, con_ty) = instantiate(self, scheme);
                // icx.obligations.extend(constraints);

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
                    self.match_pattern(icx, p, t, outer_icx, generalize_bindings)?;
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
                            self.match_pattern(icx, p, t, outer_icx, generalize_bindings)?;
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
                        self.match_pattern(icx, p, &ft, outer_icx, generalize_bindings)?;
                    }
                    Ok(())
                } else {
                    Err(TypeError::ExpectedRecord(ty.clone()))
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
        Type::Overloaded(_, _) => {
            // todo!()
        }
    }
}

fn free_env_vars(ctx: &mut TypeContext, icx: &InferCtx) -> HashSet<TypeVarId> {
    let mut acc = HashSet::new();
    for scheme in icx.type_env.values() {
        free_ty_vars(ctx, &scheme.ty, &mut acc);
        for v in &scheme.vars {
            acc.remove(v);
        }
    }
    acc
}

// ===== Instantiate / Generalize =====
pub fn instantiate(ctx: &mut TypeContext, sch: &Scheme) -> (Vec<Constraint>, Type) {
    let mut subst: HashMap<TypeVarId, TypeVarId> = HashMap::new();
    for &v in &sch.vars {
        subst.insert(v, ctx.fresh_type_var_id());
    }

    let ty = substitute(ctx, &sch.ty, &subst);

    let constraints = sch.constraints.iter().map(|c| {
        Constraint {
            name: c.name.clone(),
            params: c.params
                     .iter()
                     .map(|t| substitute(ctx, t, &subst))
                     .collect(),
        }
    }).collect();

    (constraints, ty)
}

pub fn substitute(ctx: &mut TypeContext, t: &Type, subst: &HashMap<TypeVarId, TypeVarId>) -> Type {
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
        Type::Overloaded(_, _) => {
            todo!()
        }
    }
}

pub fn generalize(ctx: &mut TypeContext, icx: &InferCtx, ty: &Type) -> Scheme {
    let mut fty = HashSet::new();
    free_ty_vars(ctx, ty, &mut fty);
    let fenv = free_env_vars(ctx, icx);
    let mut vars: Vec<TypeVarId> = fty.difference(&fenv).cloned().collect();
    vars.sort_by_key(|v| v.0);
    Scheme::poly(vars, ctx.repr(ty))
}
// pub fn generalize(ctx: &mut TypeContext, icx: &InferCtx, ty: &Type) -> Scheme {
//     let mut fty = HashSet::new();
//     free_ty_vars(ctx, ty, &mut fty);
//
//     for c in &icx.obligations {
//         for t in &c.params {
//             free_ty_vars(ctx, t, &mut fty);
//         }
//     }
//
//     let fenv = free_env_vars(ctx, icx);
//     let mut vars: Vec<TypeVarId> = fty.difference(&fenv).cloned().collect();
//     vars.sort_by_key(|v| v.0);
//
//     Scheme::new(vars, icx.obligations.clone(), ctx.repr(ty))
// }

// ===== Inference (Algorithm J core) =====
pub fn infer_item(ctx: &mut TypeContext, icx: &mut InferCtx, item: &mut Item) -> Result<Type, TypeError> {
    match item {
        Item::Stmt(stmt) => {
            infer_stmt(ctx, icx, stmt)
        }
        Item::Expr(expr) => {
            infer_expr(ctx, icx, expr)
        }
        _ => Ok(Type::con("()"))
    }
}

pub fn infer_stmt(ctx: &mut TypeContext, icx: &mut InferCtx, stmt: &mut Stmt) -> Result<Type, TypeError> {
    match stmt {
        Stmt::Let(pat, expr) => {
            let t_expr = infer_expr(ctx, icx, expr)?;
            let t_pat = ctx.fresh_type_for_pattern(pat);
            ctx.unify(&t_expr, &t_pat)?;
            let ref_icx = icx.clone(); // snapshot for reference
            ctx.match_pattern(icx, pat, &t_pat, &ref_icx, true)?;
            Ok(Type::con("()"))
        }
        Stmt::LetRec(pat, expr) => {
            match pat {
                Pat::Var(x) => {
                    let tv = Type::Var(ctx.fresh_type_var_id());
                    icx.type_env.insert(x.clone(), Scheme::mono(tv.clone()));
                    let t_expr = infer_expr(ctx, icx, expr)?;
                    ctx.unify(&tv, &t_expr)?;
                    let sch = generalize(ctx, icx, &tv);
                    icx.type_env.insert(x.clone(), sch);
                    Ok(Type::con("()"))
                }
                _ => Err(TypeError::LetRecPatternNotSupported(pat.clone())),
            }
        }
    }
}

pub fn infer_expr(ctx: &mut TypeContext, icx: &mut InferCtx, expr: &mut Expr) -> Result<Type, TypeError> {
    let ty = match &mut expr.body {
        ExprBody::Var(name) => {
            // println!("lookup {}: type_env={:?}, trait_member_env={:?}, impl_member_env={:?}",
            //          name, icx.type_env.get(name), icx.trait_member_env.get(name), icx.impl_member_env);
            let (_constraints, ty) = match icx.type_env.get(name) {
                Some(sch) => instantiate(ctx, sch),
                None => {
                    match icx.impl_member_env.get(name) {
                        None => return Err(TypeError::UnboundVariable(name.clone())),
                        Some(cands) if cands.len() == 1 => {
                            let sch = cands.iter().next().unwrap();
                            instantiate(ctx, sch)
                        }
                        Some(cands) => {
                            (vec![], Type::Overloaded(name.clone(), cands.iter().cloned().collect()))
                        }
                        // Some(cands) => {
                        //     return Err(TypeError::AmbiguousVariable {
                        //         name: name.clone(),
                        //         candidates: cands.iter().cloned().collect(),
                        //     });
                        // }
                    }
                }
            };
            ty
        }

        // ExprBody::App(f, a) => {
        //     let tf = infer_expr(ctx, icx, f)?;
        //     let ta = infer_expr(ctx, icx, a)?;
        //     let tr = Type::Var(ctx.fresh_type_var_id()); // result type variable
        //     ctx.unify(&tf, &Type::fun(ta, tr.clone()))?;
        //     tr
        // }
        ExprBody::App(f, a) => {
            let tf = infer_expr(ctx, icx, f)?;
            let ta = infer_expr(ctx, icx, a)?;
            let tr = Type::Var(ctx.fresh_type_var_id());

            match tf {
                Type::Overloaded(name, cands) => {
                    let mut filtered = Vec::new();
                    for sch in cands {
                        let (_, ty_inst) = instantiate(ctx, &sch);
                        if let Type::Fun(param, ret) = ty_inst {
                            let mut try_ctx = ctx.clone();
                            if try_ctx.unify(&param, &ta).is_ok() {
                                filtered.push((param, *ret, sch));
                            }
                        }
                    }
                    match filtered.len() {
                        0 => return Err(TypeError::NoMatchingOverload),
                        1 => {
                            let (param_ty, ret_ty, sch) = filtered.pop().unwrap();
                            ctx.unify(&param_ty, &ta)?;
                            ctx.unify(&ret_ty, &tr)?;

                            // 曖昧だった `f` の型を確定する
                            let (_, f_ty) = instantiate(ctx, &sch);
                            f.ty = Some(f_ty);

                            // App(f, a) の型
                            tr
                        }
                        _ => return Err(TypeError::AmbiguousVariable {
                            name: name.clone(), // 元の変数名を保持しておく
                            candidates: filtered.into_iter().map(|(_,_,sch)| sch).collect(),
                        }),
                    }
                }
                other => {
                    ctx.unify(&other, &Type::fun(ta, tr.clone()))?;
                    tr
                }
            }
        }

        ExprBody::Abs(pat, body) => {
            // パターンに対応する型を生成
            let t_pat = ctx.fresh_type_for_pattern(&pat);

            // 環境を拡張
            let mut icx2 = icx.clone();
            // ctx.match_pattern(&mut env2, pat, &t_pat, tenv)?;
            ctx.match_pattern(&mut icx2, &pat, &t_pat, icx, false)?;

            // 本体を推論
            let t_body = infer_expr(ctx, &mut icx2, body)?;

            // 関数型を返す
            Type::fun(t_pat, t_body)
        }

        ExprBody::Block(items) => {
            let mut icx2 = icx.clone(); // 新しいスコープ
            let mut last_ty = Type::unit();
            for item in items.iter_mut() {
                last_ty = infer_item(ctx, &mut icx2, item)?;
            }
            last_ty
        }
        ExprBody::If(cond, then_e, else_e) => {
            let t_cond = infer_expr(ctx, icx, cond)?;
            ctx.unify(&t_cond, &Type::bool_())?;

            let t_then = infer_expr(ctx, icx, then_e)?;
            let t_else = infer_expr(ctx, icx, else_e)?;
            ctx.unify(&t_then, &t_else)?;
            t_then
        }

        ExprBody::Match(scrutinee, arms) => {
            // 判別対象式の型を推論
            let t_scrut = infer_expr(ctx, icx, scrutinee)?;

            // 各アームの式型を集める
            let mut result_types = vec![];

            for (pat, body) in arms.iter_mut() {
                // パターンに対応する型を生成（型変数を含む構造）
                let t_pat = ctx.fresh_type_for_pattern(&pat);

                // scrutinee の型とパターン型を unify
                ctx.unify(&t_scrut, &t_pat)?;

                // 束縛環境を構築
                let mut env2 = icx.clone();
                ctx.match_pattern(&mut env2, &pat, &t_pat, icx, false)?;

                // アーム本体の型を推論
                let t_body = infer_expr(ctx, &mut env2, body)?;
                result_types.push(t_body);
            }

            let mut result_types = result_types.into_iter();
            let t_result = result_types.next().ok_or(TypeError::EmptyMatch)?;
            for ty in result_types {
                ctx.unify(&t_result, &ty)?;
            }

            t_result
        }

        ExprBody::Lit(Lit::Unit) => Type::unit(),
        ExprBody::Lit(Lit::Bool(_)) => Type::bool_(),
        ExprBody::Lit(Lit::Int(_)) => Type::int(),

        ExprBody::Tuple(es) => {
            let mut tys = Vec::with_capacity(es.len());
            for e in es.iter_mut() {
                let ty = infer_expr(ctx, icx, e)?;
                tys.push(ty);
            }
            Type::Tuple(tys)
        }

        ExprBody::Record(fields) => {
            // 各フィールドの型を推論
            let mut typed_fields = Vec::with_capacity(fields.len());
            for (fname, fexpr) in fields.iter_mut() {
                let t_field = infer_expr(ctx, icx, fexpr)?;
                typed_fields.push((fname.clone(), t_field));
            }
            Type::Record(typed_fields)
        }

        ExprBody::FieldAccess(base, field) => {
            let t_base = infer_expr(ctx, icx, base)?;
            match t_base {
                Type::Record(fields) => {
                    if let Some((_, ty)) = fields.iter().find(|(fname, _)| fname == field) {
                        ty.clone()
                    } else {
                        return Err(TypeError::UnknownField(field.clone(), Type::Record(fields)));
                    }
                }
                other => {
                    if let Some(con) = is_tycon(&other) {
                        let pat = Pat::con(con, vec![Pat::var("r")]);
                        let p = base.clone();
                        let mut expr = Expr::block(vec![
                            Item::Stmt(Stmt::Let(pat, p)),
                            Item::Expr(Expr::field_access(Expr::var("r"), field.clone()))
                        ]);
                        let ty = infer_expr(ctx, icx, &mut expr).map_err(|_| TypeError::ExpectedRecord(other))?;
                        ty
                    }
                    else {
                        return Err(TypeError::ExpectedRecord(other));
                    }
                }
            }
        }

        ExprBody::TupleAccess(base, index) => {
            let t_base = infer_expr(ctx, icx, base)?;
            match t_base {
                Type::Tuple(elems) => {
                    if *index < elems.len() {
                        elems[*index].clone()
                    } else {
                        return Err(TypeError::IndexOutOfBounds(*index, Type::Tuple(elems)));
                    }
                }
                other => {
                    if let Some(con) = is_tycon(&other) {
                        let pat = Pat::con(con, vec![Pat::var("t")]);
                        let p = base.clone();
                        let mut expr = Expr::block(vec![
                            Item::Stmt(Stmt::Let(pat, p)),
                            Item::Expr(Expr::tuple_access(Expr::var("t"), *index))
                        ]);
                        let ty = infer_expr(ctx, icx, &mut expr).map_err(|_| TypeError::ExpectedTuple(other))?;
                        ty
                    }
                    else {
                        return Err(TypeError::ExpectedTuple(other));
                    }
                }
            }
        }

        ExprBody::RawTraitRecord(_) => {
            unreachable!()
        }
    };
    expr.ty = Some(ty.clone());
    Ok(ty)
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

    env.insert("()".into(), Kind::Star);
    env.insert("Int".into(), Kind::Star);
    env.insert("Bool".into(), Kind::Star);
    env.insert("List".into(), Kind::Fun(Box::new(Kind::Star), Box::new(Kind::Star)));
    env.insert("Option".into(), Kind::Fun(Box::new(Kind::Star), Box::new(Kind::Star)));

    env
}

pub fn initial_type_env(ctx: &mut TypeContext) -> TypeEnv {
    let mut type_env = TypeEnv::new();

    // None : ∀a. Option a
    let a = ctx.fresh_type_var_id();
    type_env.insert(
        "None".into(),
        Scheme {
            vars: vec![a],
            constraints: vec![],
            ty: Type::app(
                Type::con("Option"),
                Type::var(a),
            ),
        },
    );

    // Some : ∀a. a -> Option a
    let a = ctx.fresh_type_var_id();
    type_env.insert(
        "Some".into(),
        Scheme {
            vars: vec![a],
            constraints: vec![],
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
    type_env.insert(
        "Nil".into(),
        Scheme {
            vars: vec![a],
            constraints: vec![],
            ty: Type::app(Type::con("List"), Type::var(a)),
        },
    );

    // Cons : ∀a. a -> List a -> List a
    let a = ctx.fresh_type_var_id();
    type_env.insert(
        "Cons".into(),
        Scheme {
            vars: vec![a],
            constraints: vec![],
            ty: Type::fun(
                Type::var(a),
                Type::fun(
                    Type::app(Type::con("List"), Type::var(a)),
                    Type::app(Type::con("List"), Type::var(a)),
                ),
            ),
        },
    );

    // // map : ∀a b. (a -> b) -> List a -> List b
    // let a = ctx.fresh_type_var_id();
    // let b = ctx.fresh_type_var_id();

    // type_env.insert(
    //     "map".into(),
    //     Scheme {
    //         vars: vec![a, b],
    //         constraints: vec![],
    //         ty: Type::fun(
    //             Type::fun(Type::var(a), Type::var(b)), // (a -> b)
    //             Type::fun(
    //                 Type::app(Type::con("List"), Type::var(a)), // List a
    //                 Type::app(Type::con("List"), Type::var(b)), // List b
    //             ),
    //         ),
    //     },
    // );

    let a = ctx.fresh_type_var_id();
    type_env.insert(
        "__builtin_==__".into(),
        Scheme {
            vars: vec![a],
            constraints: vec![],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::con("Bool"))),
        },
    );

    let a = ctx.fresh_type_var_id();
    type_env.insert(
        "__builtin_!=__".into(),
        Scheme {
            vars: vec![a],
            constraints: vec![],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::con("Bool"))),
        },
    );

    let a = ctx.fresh_type_var_id();
    type_env.insert(
        "__builtin_<__".into(),
        Scheme {
            vars: vec![a],
            constraints: vec![],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::con("Bool"))),
        },
    );


    let a = ctx.fresh_type_var_id();
    type_env.insert(
        "__builtin_<=__".into(),
        Scheme {
            vars: vec![a],
            constraints: vec![],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::con("Bool"))),
        },
    );

    let a = ctx.fresh_type_var_id();
    type_env.insert(
        "__builtin_>__".into(),
        Scheme {
            vars: vec![a],
            constraints: vec![],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::con("Bool"))),
        },
    );

    let a = ctx.fresh_type_var_id();
    type_env.insert(
        "__builtin_>=__".into(),
        Scheme {
            vars: vec![a],
            constraints: vec![],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::con("Bool"))),
        },
    );

    let a = ctx.fresh_type_var_id();
    type_env.insert(
        "__builtin_+__".into(),
        Scheme {
            vars: vec![a],
            constraints: vec![],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::var(a))),
        },
    );

    let a = ctx.fresh_type_var_id();
    type_env.insert(
        "__builtin_-__".into(),
        Scheme {
            vars: vec![a],
            constraints: vec![],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::var(a))),
        },
    );

    let a = ctx.fresh_type_var_id();
    type_env.insert(
        "__builtin_*__".into(),
        Scheme {
            vars: vec![a],
            constraints: vec![],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::var(a))),
        },
    );

    let a = ctx.fresh_type_var_id();
    type_env.insert(
        "__builtin_/__".into(),
        Scheme {
            vars: vec![a],
            constraints: vec![],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::var(a))),
        },
    );

    let a = ctx.fresh_type_var_id();
    type_env.insert(
        "negate".into(),
        Scheme {
            vars: vec![a],
            constraints: vec![],
            ty: Type::fun(Type::var(a), Type::var(a)),
        },
    );

    let a = ctx.fresh_type_var_id();
    type_env.insert(
        "not".into(),
        Scheme {
            vars: vec![a],
            constraints: vec![],
            ty: Type::fun(Type::var(a), Type::var(a)),
        },
    );

    type_env
}
