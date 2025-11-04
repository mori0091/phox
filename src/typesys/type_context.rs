use super::*;
use crate::syntax::ast::*;

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
    pub fn find(&mut self, id: TypeVarId) -> TypeVarId {
        let p = self.parent[id.0];
        if p != id {
            let root = self.find(p);
            self.parent[id.0] = root;
        }
        self.parent[id.0]
    }

    pub fn get_bound(&mut self, v: &TypeVarId) -> Option<Type> {
        let r = self.find(*v);
        self.binding[r.0].clone()
    }
}

impl TypeContext {
    // Occurs check: does tv occur in ty (following bindings)?
    pub fn occurs_in(&mut self, tv: TypeVarId, ty: &Type) -> bool {
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
    pub fn unify(&mut self, a: &Type, b: &Type) -> Result<(), TypeError> {
        match (a, b) {
            (Type::Var(v), t) | (t, Type::Var(v)) => {
                self.unify_var(*v, t)
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

            _ => {
                // eprintln!("unify failed: {a:?} vs {b:?}");
                Err(TypeError::Mismatch(a.clone(), b.clone()))
            }
        }
    }

    fn unify_var(&mut self, v: TypeVarId, t: &Type) -> Result<(), TypeError> {
        // 代表を取る
        let r = self.find(v);

        // 自分自身とのユニ化は無視
        if let Type::Var(w) = t {
            let rw = self.find(*w);
            if r == rw {
                return Ok(());
            }
        }

        // 代表に束縛があるなら、その束縛と t を統合
        if let Some(bound) = self.binding[r.0].clone() {
            return self.unify(&bound, t);
        }

        // t 側が Var で束縛があるなら連結（Var vs Var の統合）
        if let Type::Var(w) = t {
            let rw = self.find(*w);
            if let Some(bound_w) = self.binding[rw.0].clone() {
                return self.unify(&Type::Var(r), &bound_w);
            }
            // どちらも未束縛 → 代表を連結
            self.parent[r.0] = rw;
            return Ok(());
        }

        // occurs check を生の t に対して実施
        if self.occurs_in(r, t) {
            return Err(TypeError::RecursiveType);
        }

        // ★ ここで repr はかけず、そのまま束縛。必要なら clone。
        self.binding[r.0] = Some(t.clone());
        Ok(())
    }
}

impl TypeContext {
    pub fn fresh_type_for_pattern(&mut self, pat: &Pat) -> Type {
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
    pub fn match_pattern(
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
                self.unify(&expected, ty)
            }

            Pat::Var(x) => {
                let sch = if generalize_bindings {
                    generalize(self, &outer_icx, ty)   // let の場合
                } else {
                    // TypeScheme::mono(ty.repr(self))       // Abs や match の場合は単相
                    TypeScheme::mono(ty.clone()) // Abs や match の場合は単相
                };
                icx.type_env.insert(x.clone(), sch);
                Ok(())
            }

            Pat::Con(name, args) => {
                let scheme = icx.type_env.get(name).ok_or(TypeError::UnknownConstructor(name.clone()))?;
                let (_constraints, con_ty) = scheme.instantiate(self);
                // icx.obligations.extend(constraints);

                let mut arg_types = Vec::new();
                let mut ty_fun = con_ty;

                for _ in args {
                    match ty_fun.repr(self) {
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
                let ty = ty.repr(self);
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
