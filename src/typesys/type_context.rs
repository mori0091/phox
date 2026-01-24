use super::*;
use crate::module::*;
use crate::syntax::ast::*;

// ===== TypeContext: union-find + binding =====
pub type TypeContext = Context<TypeVarId, Type>;

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
            Type::Fun(a, b) => {
                self.occurs_in(tv, a) || self.occurs_in(tv, b)
            }
            Type::App(a, b) => {
                self.occurs_in(tv, a.as_ref_type()) || self.occurs_in(tv, b.as_ref_type())
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
    pub fn unify(&mut self, a: &Type, b: &Type) -> Result<(), Error> {
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
                self.unify(f1.as_ref_type(), f2.as_ref_type())?;
                self.unify(x1.as_ref_type(), x2.as_ref_type())
            }

            (Type::Tuple(ts1), Type::Tuple(ts2)) => {
                if ts1.len() != ts2.len() {
                    return Err(Error::TupleLengthMismatch(ts1.len(), ts2.len()));
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
                    return Err(Error::TypeMismatch(
                        Type::Record(fields1.clone()),
                        Type::Record(fields2.clone()),
                    ));
                }

                // 名前で対応付けて unify
                for (fname, ty1) in fields1 {
                    match fields2.iter().find(|(n, _)| n == fname) {
                        Some((_, ty2)) => self.unify(ty1, ty2)?,
                        None => {
                            return Err(Error::UnknownField(
                                fname.clone(),
                                Type::Record(fields2.clone())
                            ));
                        }
                    }
                }

                Ok(())
            }

            _ => {
                // eprintln!("unify failed: {a:?} vs {b:?}");
                Err(Error::TypeMismatch(a.clone(), b.clone()))
            }
        }
    }

    fn unify_var(&mut self, v: TypeVarId, t: &Type) -> Result<(), Error> {
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
            if self.non_touchable.contains(&r) && !self.non_touchable.contains(&rw) {
                self.parent[rw.0] = r;
                return Ok(());
            }
            else {
                self.parent[r.0] = rw;
                return Ok(());
            }
        }

        // occurs check を生の t に対して実施
        if self.occurs_in(r, t) {
            return Err(Error::RecursiveType);
        }

        {
            if self.non_touchable.contains(&r) {
                panic!("[BUG] non-touchable var is about to be bound: r={:?}, t={:?}", r, t);
            }
            if let Type::Var(w) = t {
                let rw = self.find(*w);
                if self.non_touchable.contains(&rw) {
                    panic!("[BUG] non-touchable target var in Var-Var binding: r={:?}, rw={:?}", r, rw);
                }
            }
        }

        // ★ ここで repr はかけず、そのまま束縛。必要なら clone。
        // {
        //     eprintln!("[unify_var] BIND r={:?} := {:?}", r, t);
        // }
        self.binding[r.0] = Some(t.clone());
        Ok(())
    }
}

impl TypeContext {
    pub fn fresh_type_for_pattern(&mut self, pat: &Pat) -> Type {
        match pat {
            Pat::Var(_) | Pat::Wildcard => Type::Var(self.fresh_var_id()),
            Pat::Lit(lit) => match lit {
                Lit::Unit => Type::unit(),
                Lit::Bool(_) => Type::bool_(),
                Lit::Int(_) => Type::int(),
            },
            Pat::Tuple(ps) => {
                let ts = ps.iter().map(|p| self.fresh_type_for_pattern(p)).collect();
                Type::Tuple(ts)
            }
            Pat::Con(_, _) => {
                Type::Var(self.fresh_var_id()) // パターン全体の型は未知とする
            }
            Pat::Record(fields) => {
                let tys = fields.iter()
                                .map(|(name, _p)| (name.clone(), Type::Var(self.fresh_var_id())))
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
        monotype_bindings: bool,
        bindings: &mut Vec<(Symbol, Type)>,
    ) -> Result<Vec<Constraint>, Error> {
        let mut constraints = Vec::new();
        match pat {
            Pat::Wildcard => {}, // 束縛なし

            Pat::Lit(lit) => {
                let expected = match lit {
                    Lit::Unit => Type::unit(),
                    Lit::Bool(_) => Type::bool_(),
                    Lit::Int(_) => Type::int(),
                };
                constraints.push(Constraint::type_eq(&expected, ty));
            }

            Pat::Var(x) => {
                if monotype_bindings {
                    let sch = TypeScheme::mono(ty.clone());
                    icx.put_type_scheme(x.clone(), sch);
                }
                else {
                    bindings.push((x.clone(), ty.clone()));
                }
            }

            Pat::Con(name, args) => {
                let scheme = icx.get_type_scheme(name).ok_or(Error::UnknownConstructor(name.clone()))?;
                let (cs, con_ty) = scheme.instantiate(self);
                let mut css = cs.into_vec();

                let mut arg_types = Vec::new();
                let mut ty_fun = con_ty;

                for _ in args {
                    match ty_fun.repr(self) {
                        Type::Fun(a, b) => {
                            arg_types.push(*a);
                            ty_fun = *b;
                        }
                        other => return Err(Error::ConstructorArityMismatch(name.clone(), args.len(), other)),
                    }
                }

                css.push(Constraint::type_eq(&ty_fun, ty));

                for (p, t) in args.iter().zip(arg_types.iter()) {
                    let cs = self.match_pattern(icx, p, t, monotype_bindings, bindings)?;
                    css.extend(cs);
                }

                constraints.extend(css.drain(..));
            }

            Pat::Tuple(ps) => {
                match ty.repr(self) {
                    Type::Tuple(existing_elems) => {
                        if existing_elems.len() != ps.len() {
                            return Err(Error::TupleLengthMismatch(ps.len(), existing_elems.len()));
                        }

                        for (p, elem_ty) in ps.iter().zip(existing_elems.iter()) {
                            let cs = self.match_pattern(icx, p, elem_ty, monotype_bindings, bindings)?;
                            constraints.extend(cs);
                        }
                    }
                    _ => {
                        let mut ts = Vec::new();
                        for _p in ps {
                            ts.push(Type::Var(self.fresh_var_id()));
                        }

                        let mut css = Vec::new();
                        css.push(Constraint::type_eq(&Type::Tuple(ts.clone()), ty));
                        for (p, t) in ps.iter().zip(ts.iter()) {
                            let cs = self.match_pattern(icx, p, t, monotype_bindings, bindings)?;
                            css.extend(cs);
                        }

                        constraints.extend(css.drain(..));
                    }
                }
            }

            Pat::Record(fields) => {
                match ty.repr(self) {
                    Type::Record(existing_fields) => {
                        for (fname, p) in fields {
                            let (_, field_ty) = existing_fields
                                .iter()
                                .find(|(n, _)| n == fname)
                                .ok_or_else(|| Error::UnknownField(fname.clone(), ty.clone()))?;

                            // ここで field_ty をそのまま使う
                            let cs = self.match_pattern(icx, p, field_ty, monotype_bindings, bindings)?;
                            constraints.extend(cs);
                        }
                    }

                    _ => {
                        let mut tys = Vec::new();
                        for (n, _p) in fields {
                            tys.push((n.clone(), Type::Var(self.fresh_var_id())));
                        }

                        let mut css = Vec::new();
                        css.push(Constraint::type_eq(&Type::Record(tys.clone()), ty));
                        for (fname, p) in fields {
                            let ft = tys.iter()
                                        .find(|(n, _)| n == fname)
                                        .ok_or_else(|| Error::UnknownField(fname.clone(), ty.clone()))?
                                        .1.repr(self);
                            let cs = self.match_pattern(icx, p, &ft, monotype_bindings, bindings)?;
                            css.extend(cs);
                        }

                        constraints.extend(css.drain(..));
                    }
                }
            }
        }
        Ok(constraints)
    }
}
