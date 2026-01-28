use indexmap::IndexSet;
use std::fmt;

use super::*;
use crate::typesys::*;
use crate::module::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Expr {
    pub span: (usize, usize),
    pub body: ExprBody,
    pub ty: Option<Type>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ExprBody {
    Lit(Lit),
    Var(Symbol),
    App(Box<Expr>, Box<Expr>),

    Abs(Pat, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Vec<(Pat, Expr)>),
    Block(Vec<Item>),               // ex. `{stmt; stmt; expr; expr}`

    Tuple(Vec<Expr>),               // ex. `(1,)`, `(1, true, ())`
    TupleAccess(Box<Expr>, usize),  // ex. `p.0`

    RawTraitRecord(RawTraitHead),   // ex. `@{ Eq Int }`
    TraitRecord(TraitHead),
    Record(Vec<(String, Expr)>),    // ex. `@{ x:a, y:b }`
    FieldAccess(Box<Expr>, String), // ex. `p.x`
}

impl Expr {
    pub fn is_value(&self) -> bool {
        match &self.body {
            // values
            ExprBody::Lit(_)            => true,
            ExprBody::Abs(_, _)         => true,
            ExprBody::Tuple(_)          => true,
            ExprBody::RawTraitRecord(_) => true,
            ExprBody::TraitRecord(_)    => true,
            ExprBody::Record(_)         => true,

            // non-values (expr may be sustituted by evaluation)
            ExprBody::Var(_)            => false,
            ExprBody::App(_, _)         => false,
            ExprBody::If(_, _, _)       => false,
            ExprBody::Match(_, _)       => false,
            ExprBody::Block(_)          => false,
            ExprBody::TupleAccess(_, _) => false,
            ExprBody::FieldAccess(_, _) => false,
        }
    }
}

impl Expr {
    pub fn lit(x: Lit) -> Self {
        Expr { span:(0,0), body: ExprBody::Lit(x), ty: None }
    }
    pub fn unit() -> Self {
        Expr::lit(Lit::Unit)
    }
    pub fn int(x: i64) -> Self {
        Expr::lit(Lit::Int(x))
    }
    pub fn bool_(b: bool) -> Self {
        Expr::lit(Lit::Bool(b))
    }

    pub fn expr(e: ExprBody) -> Self {
        Expr { span:(0,0), body: e, ty: None }
    }
    pub fn unresolved_var<S: Into<String>>(s: S) -> Self {
        Expr::expr(ExprBody::Var(Symbol::unresolved(s)))
    }
    pub fn unresolved_qvar(path: Path) -> Self {
        Expr::expr(ExprBody::Var(Symbol::Unresolved(path)))
    }
    pub fn app(f: Expr, x: Expr) -> Self {
        Expr::expr(ExprBody::App(Box::new(f), Box::new(x)))
    }
    pub fn abs(pat: Pat, e: Expr) -> Self {
        Expr::expr(ExprBody::Abs(pat, Box::new(e)))
    }
    pub fn if_(e1: Expr, e2: Expr, e3: Expr) -> Self {
        Expr::expr(ExprBody::If(Box::new(e1), Box::new(e2), Box::new(e3)))
    }
    pub fn match_(e: Expr, arms: Vec<(Pat, Expr)>) -> Self {
        Expr::expr(ExprBody::Match(Box::new(e), arms))
    }

    pub fn record(fields: Vec<(String, Expr)>) -> Self {
        Expr::expr(ExprBody::Record(fields))
    }

    pub fn tuple(elems: Vec<Expr>) -> Self {
        Expr::expr(ExprBody::Tuple(elems))
    }

    pub fn raw_trait_record(raw: RawTraitHead) -> Self {
        Expr::expr(ExprBody::RawTraitRecord(raw))
    }

    pub fn field_access<S: Into<String>>(e: Expr, s: S) -> Self {
        Expr::expr(ExprBody::FieldAccess(Box::new(e), s.into()))
    }

    pub fn tuple_access(e: Expr, index: usize) -> Self {
        Expr::expr(ExprBody::TupleAccess(Box::new(e), index))
    }

    pub fn block(items: Vec<Item>) -> Self {
        Expr::expr(ExprBody::Block(items))
    }
}

// ----------------------------------------------
// FreeTypeVars
impl FreeVars for Expr {
    fn free_vars(&self, ctx: &mut UnifiedContext, acc: &mut IndexSet<Var>) {
        match &self.ty {
            Some(ty) => {
                ty.free_vars(ctx, acc);
            }
            None => {}
        }
        match &self.body {
            ExprBody::Lit(_) => {}
            ExprBody::Var(_) => {}
            ExprBody::App(f, x) => {
                f.free_vars(ctx, acc);
                x.free_vars(ctx, acc);
            }

            ExprBody::Abs(_pat, e) => {
                e.free_vars(ctx, acc);
            }
            ExprBody::If(cond_expr, then_expr, else_expr) => {
                cond_expr.free_vars(ctx, acc);
                then_expr.free_vars(ctx, acc);
                else_expr.free_vars(ctx, acc);
            }
            ExprBody::Match(expr, arms) => {
                expr.free_vars(ctx, acc);
                for (_pat, e) in arms {
                    e.free_vars(ctx, acc);
                }
            }
            ExprBody::Block(items) => {
                for item in items {
                    item.free_vars(ctx, acc);
                }
            }

            ExprBody::Tuple(es) => {
                for e in es {
                    e.free_vars(ctx, acc);
                }
            }
            ExprBody::TupleAccess(base, _index) => {
                base.free_vars(ctx, acc);
            }

            ExprBody::RawTraitRecord(_) => {}
            ExprBody::TraitRecord(head) => {
                head.free_vars(ctx, acc);
            }
            ExprBody::Record(fields) => {
                for (_name, e) in fields {
                    e.free_vars(ctx, acc);
                }
            }
            ExprBody::FieldAccess(base, _field_label) => {
                base.free_vars(ctx, acc);
            }
        }
    }
}

// ----------------------------------------------
// Repr
impl Repr for Expr {
    fn repr(&self, ctx: &mut TypeContext) -> Self {
        let span = self.span.clone();
        let ty = match &self.ty {
            Some(ty) => Some(ty.repr(ctx)),
            None => None,
        };
        let Expr {body, ..} = match &self.body {
            ExprBody::Lit(_) => { self.clone() }
            ExprBody::Var(_) => { self.clone() }
            ExprBody::App(f, x) => {
                let f = f.repr(ctx);
                let x = x.repr(ctx);
                Expr::app(f, x)
            }

            ExprBody::Abs(pat, e) => {
                let pat = pat.clone();
                let e = e.repr(ctx);
                Expr::abs(pat, e)
            }
            ExprBody::If(cond_expr, then_expr, else_expr) => {
                let cond_expr = cond_expr.repr(ctx);
                let then_expr = then_expr.repr(ctx);
                let else_expr = else_expr.repr(ctx);
                Expr::if_(cond_expr, then_expr, else_expr)
            }
            ExprBody::Match(expr, arms) => {
                let expr = expr.repr(ctx);
                let arms = arms.iter().map(|(p, e)| (p.clone(), e.repr(ctx))).collect();
                Expr::match_(expr, arms)
            }
            ExprBody::Block(items) => {
                let items = items.iter().map(|item| item.repr(ctx)).collect();
                Expr::block(items)
            }

            ExprBody::Tuple(es) => {
                let es = es.iter().map(|e| e.repr(ctx)).collect();
                Expr::tuple(es)
            }
            ExprBody::TupleAccess(base, index) => {
                let base = base.repr(ctx);
                Expr::tuple_access(base, *index)
            }

            ExprBody::RawTraitRecord(_) => { self.clone() }
            ExprBody::TraitRecord(head) => {
                let head = head.repr(ctx);
                Expr::expr(ExprBody::TraitRecord(head))
            }
            ExprBody::Record(fields) => {
                let fields = fields.iter().map(|(name, e)| (name.clone(), e.repr(ctx))).collect();
                Expr::record(fields)
            }
            ExprBody::FieldAccess(base, field_label) => {
                let base = base.repr(ctx);
                let field_label = field_label.clone();
                Expr::field_access(base, field_label)
            }
        };
        Expr { span, body, ty }
    }
}

// ----------------------------------------------
// ApplySubst
impl ApplySubst for Expr {
    fn apply_subst(&self, subst: &Subst) -> Self {
        let span = self.span.clone();
        let ty = match &self.ty {
            Some(ty) => Some(ty.apply_subst(subst)),
            None => None,
        };
        let Expr {body, ..} = match &self.body {
            ExprBody::Lit(_) => { self.clone() }
            ExprBody::Var(_) => { self.clone() }
            ExprBody::App(f, x) => {
                let f = f.apply_subst(subst);
                let x = x.apply_subst(subst);
                Expr::app(f, x)
            }

            ExprBody::Abs(pat, e) => {
                let pat = pat.clone();
                let e = e.apply_subst(subst);
                Expr::abs(pat, e)
            }
            ExprBody::If(cond_expr, then_expr, else_expr) => {
                let cond_expr = cond_expr.apply_subst(subst);
                let then_expr = then_expr.apply_subst(subst);
                let else_expr = else_expr.apply_subst(subst);
                Expr::if_(cond_expr, then_expr, else_expr)
            }
            ExprBody::Match(expr, arms) => {
                let expr = expr.apply_subst(subst);
                let arms = arms.iter().map(|(p, e)| (p.clone(), e.apply_subst(subst))).collect();
                Expr::match_(expr, arms)
            }
            ExprBody::Block(items) => {
                let items = items.iter().map(|item| item.apply_subst(subst)).collect();
                Expr::block(items)
            }

            ExprBody::Tuple(es) => {
                let es = es.iter().map(|e| e.apply_subst(subst)).collect();
                Expr::tuple(es)
            }
            ExprBody::TupleAccess(base, index) => {
                let base = base.apply_subst(subst);
                Expr::tuple_access(base, *index)
            }

            ExprBody::RawTraitRecord(_) => { self.clone() }
            ExprBody::TraitRecord(head) => {
                let head = head.apply_subst(subst);
                Expr::expr(ExprBody::TraitRecord(head))
            }
            ExprBody::Record(fields) => {
                let fields = fields.iter().map(|(name, e)| (name.clone(), e.apply_subst(subst))).collect();
                Expr::record(fields)
            }
            ExprBody::FieldAccess(base, field_label) => {
                let base = base.apply_subst(subst);
                let field_label = field_label.clone();
                Expr::field_access(base, field_label)
            }
        };
        Expr { span, body, ty }
    }
}

// ----------------------------------------------
// SchemePretty

// ----------------------------------------------
// Pretty

// ----------------------------------------------
// fmt::Display
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.body {
            ExprBody::Lit(a)            => write!(f, "{}", a),
            // ExprBody::Var(x)            => write!(f, "{}", x),
            ExprBody::Var(x)            => write!(f, "{}", x.pretty()),
            ExprBody::Abs(x, e)         => write!(f, "λ{}.{}", x, e),
            ExprBody::App(e1, e2) => {
                match e2.body {
                    ExprBody::Abs(_, _) | ExprBody::App(_, _) => {
                        write!(f, "{} ({})", e1, e2)
                    }
                    _ => {
                        write!(f, "{} {}", e1, e2)
                    }
                }
            }
            ExprBody::If(e1, e2, e3)    => write!(f, "if ({}) {} else {}", e1, e2, e3),
            ExprBody::Match(expr, arms) => {
                let s: Vec<String>
                    = arms.iter()
                          .map(|(p, e)| format!("  {} => {},", p, e))
                          .collect();
                write!(f, "match ({}) {{\n{}\n}}", *expr, s.join("\n"))
            }
            ExprBody::Tuple(es) => {
                assert!(!es.is_empty());
                if es.len() == 1 {
                    write!(f, "({},)", es[0])
                }
                else {
                    let s: Vec<String> = es.iter().map(|t| t.to_string()).collect();
                    write!(f, "({})", s.join(", "))
                }
            }
            ExprBody::Record(fields) => {
                if fields.is_empty() {
                    write!(f, "@{{}}")
                }
                else {
                    let s: Vec<String>
                        = fields.iter()
                                .map(|(k, v)| format!("{}: {}", k, v))
                                .collect();
                    write!(f, "@{{ {} }}", s.join(", "))
                }
            }
            ExprBody::RawTraitRecord(raw_constraint) => {
                write!(f, "@{{ {:?} }}", raw_constraint)
            }
            ExprBody::TraitRecord(head) => {
                write!(f, "@{{{}}}", head.pretty())
            }
            ExprBody::FieldAccess(base, field) => {
                match base.body {
                    ExprBody::Abs(_,_) | ExprBody::App(_,_) | ExprBody::If(_,_,_) => {
                        write!(f, "({}).{}", base, Symbol::local(field).pretty())
                    }
                    _ => write!(f, "{}.{}", base, Symbol::local(field).pretty())
                }
            }
            ExprBody::TupleAccess(base, index) => {
                match base.body {
                    ExprBody::Abs(_,_) | ExprBody::App(_,_) | ExprBody::If(_,_,_) => {
                        write!(f, "({}).{}", base, index)
                    }
                    _ => write!(f, "{}.{}", base, index)
                }
            }
            ExprBody::Block(items) => {
                let s: Vec<_> = items.iter().map(|e| format!("  {}", e)).collect();
                write!(f, "{{\n{}\n}}", s.join(";\n"))
            }
        }
    }
}

// ----------------------------------------------
// SchemePretty
impl RenameForPretty for Expr {
    fn rename_var(&self, map: &mut VarNameMap) -> Self {
        let span = self.span.clone();
        let ty = match &self.ty {
            Some(ty) => Some(ty.rename_var(map)),
            None => None,
        };
        let Expr {body, ..} = match &self.body {
            ExprBody::Lit(_) => { self.clone() }
            ExprBody::Var(_) => { self.clone() }
            ExprBody::App(f, x) => {
                let f = f.rename_var(map);
                let x = x.rename_var(map);
                Expr::app(f, x)
            }

            ExprBody::Abs(pat, e) => {
                let pat = pat.clone();
                let e = e.rename_var(map);
                Expr::abs(pat, e)
            }
            ExprBody::If(cond_expr, then_expr, else_expr) => {
                let cond_expr = cond_expr.rename_var(map);
                let then_expr = then_expr.rename_var(map);
                let else_expr = else_expr.rename_var(map);
                Expr::if_(cond_expr, then_expr, else_expr)
            }
            ExprBody::Match(expr, arms) => {
                let expr = expr.rename_var(map);
                let arms = arms.iter().map(|(p, e)| (p.clone(), e.rename_var(map))).collect();
                Expr::match_(expr, arms)
            }
            ExprBody::Block(items) => {
                let items = items.iter().map(|item| item.rename_var(map)).collect();
                Expr::block(items)
            }

            ExprBody::Tuple(es) => {
                let es = es.iter().map(|e| e.rename_var(map)).collect();
                Expr::tuple(es)
            }
            ExprBody::TupleAccess(base, index) => {
                let base = base.rename_var(map);
                Expr::tuple_access(base, *index)
            }

            ExprBody::RawTraitRecord(_) => { self.clone() }
            ExprBody::TraitRecord(head) => {
                let head = head.rename_var(map);
                Expr::expr(ExprBody::TraitRecord(head))
            }
            ExprBody::Record(fields) => {
                let fields = fields.iter().map(|(name, e)| (name.clone(), e.rename_var(map))).collect();
                Expr::record(fields)
            }
            ExprBody::FieldAccess(base, field_label) => {
                let base = base.rename_var(map);
                let field_label = field_label.clone();
                Expr::field_access(base, field_label)
            }
        };
        Expr { span, body, ty }
    }
}
