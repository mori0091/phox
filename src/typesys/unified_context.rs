use indexmap::IndexSet;
use crate::module::*;
use crate::syntax::ast::*;

use super::*;

#[derive(Clone)]
pub struct UnifiedContext {
    pub non_touchable: IndexSet<Var>,
    pub ty: TypeContext,
    // pub row: RowContext,
    // pub nat: NatContext,
}
impl UnifiedContext {
    pub fn new() -> Self {
        Self {
            non_touchable: IndexSet::new(),
            ty: TypeContext::new(),
        }
    }

    pub fn set_non_touchable(&mut self, non_touchable: &IndexSet<Var>) {
        self.non_touchable.extend(non_touchable);
        self.ty.non_touchable.extend(non_touchable.iter().filter_map(|v| {
            match v {
                Var::Ty(id) => Some(id),
                // _ => None,
            }
        }));
    }

    pub fn clear_non_touchable(&mut self) {
        self.non_touchable.clear();
        self.ty.non_touchable.clear();
    }
}

impl UnifiedContext {
    pub fn fresh_type_for_pattern(&mut self, pat: &Pat) -> Type {
        match pat {
            Pat::Var(_) | Pat::Wildcard => Type::Var(self.ty.fresh_var_id()),
            Pat::Lit(lit) => match lit {
                Lit::Unit    => Type::unit(),
                Lit::Bool(_) => Type::bool_(),
                Lit::Int(_)  => Type::int(),
                Lit::U8(_)   => Type::u8_(),
                Lit::U16(_)  => Type::u16_(),
                Lit::U32(_)  => Type::u32_(),
                Lit::U64(_)  => Type::u64_(),
            },
            Pat::Tuple(ps) => {
                let ts = ps.iter().map(|p| self.fresh_type_for_pattern(p)).collect();
                Type::Tuple(ts)
            }
            Pat::Con(_, _) => {
                Type::Var(self.ty.fresh_var_id()) // パターン全体の型は未知とする
            }
            Pat::Record(fields) => {
                let tys = fields.iter()
                                .map(|(name, _p)| (name.clone(), Type::Var(self.ty.fresh_var_id())))
                                .collect();
                Type::Record(tys)
            }
            Pat::Array(_ps, _rest) => {
                let ty = Type::Var(self.ty.fresh_var_id());
                Type::array(ty)
            }
        }
    }
}

impl UnifiedContext {
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
                    Lit::Unit    => Type::unit(),
                    Lit::Bool(_) => Type::bool_(),
                    Lit::Int(_)  => Type::int(),
                    Lit::U8(_)   => Type::u8_(),
                    Lit::U16(_)  => Type::u16_(),
                    Lit::U32(_)  => Type::u32_(),
                    Lit::U64(_)  => Type::u64_(),
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
                            ts.push(Type::Var(self.ty.fresh_var_id()));
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
                            tys.push((n.clone(), Type::Var(self.ty.fresh_var_id())));
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

            Pat::Array(ps, rest) => {
                let ty = ty.repr(self);
                match ty {
                    Type::Array(ref elem_ty) => {
                        for p in ps.iter() {
                            let cs = self.match_pattern(icx, p, &*elem_ty, monotype_bindings, bindings)?;
                            constraints.extend(cs);
                        }
                        if let Some(PatRest::Named(sym)) = rest {
                            let cs = self.match_pattern(icx, &Pat::Var(sym.clone()), &ty, monotype_bindings, bindings)?;
                            constraints.extend(cs);
                        }
                    }
                    _ => {
                        let elem_ty = Type::Var(self.ty.fresh_var_id());
                        let array_ty = Type::array(elem_ty.clone());

                        let mut css = Vec::new();
                        css.push(Constraint::type_eq(&array_ty, &ty));
                        for p in ps.iter() {
                            let cs = self.match_pattern(icx, p, &elem_ty, monotype_bindings, bindings)?;
                            css.extend(cs);
                        }
                        if let Some(PatRest::Named(sym)) = rest {
                            let cs = self.match_pattern(icx, &Pat::Var(sym.clone()), &array_ty, monotype_bindings, bindings)?;
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
