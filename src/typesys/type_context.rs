use super::*;
use crate::module::*;

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
                if let Some(bound) = self.get_bound(&r) {
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
            Type::Array(ty) => {
                self.occurs_in(tv, ty)
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
        if let (Type::Con(c), Type::App(f, x)) | (Type::App(f, x), Type::Con(c)) = (a, b) {
            if c == &Symbol::unicode_scalar_string() {
                self.unify(f.as_ref_type(), &Type::Con(Symbol::str_()))?;
                self.unify(x.as_ref_type(), &Type::Con(Symbol::unicode_scalar_value()))?;
                return Ok(())
            }
        }

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

            (Type::Array(t1), Type::Array(t2)) => {
                self.unify(t1, t2)?;
                Ok(())
            }
            (Type::Array(ty), Type::App(f, x)) |
            (Type::App(f, x), Type::Array(ty)) => {
                self.unify(f.as_ref_type(), &Type::Con(Symbol::array()))?;
                self.unify(ty, x.as_ref_type())?;
                Ok(())
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
        if let Some(bound) = self.get_bound(&r) {
            return self.unify(&bound, t);
        }

        // t 側が Var で束縛があるなら連結（Var vs Var の統合）
        if let Type::Var(w) = t {
            let rw = self.find(*w);

            if let Some(bound_w) = self.get_bound(&rw) {
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
        self.bind(&r, t);
        Ok(())
    }
}
