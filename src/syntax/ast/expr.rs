use std::fmt;

use super::*;
use crate::typesys::Type;
use crate::module::*;

#[derive(Clone, Debug)]
pub struct Expr {
    pub body: ExprBody,
    pub ty: Option<Type>,
}

#[derive(Clone, Debug)]
pub enum ExprBody {
    Lit(Lit),
    Var(Symbol),
    App(Box<Expr>, Box<Expr>),

    Abs(Pat, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Vec<(Pat, Expr)>),

    Tuple(Vec<Expr>),               // ex. `(1,)`, `(1, true, ())`
    Record(Vec<(String, Expr)>),    // ex. `@{ x:a, y:b }`
    RawTraitRecord(RawTraitHead),   // ex. `@{ Eq Int }`
    FieldAccess(Box<Expr>, String), // ex. `p.x`
    TupleAccess(Box<Expr>, usize),  // ex. `p.0`
    Block(Vec<Item>),               // ex. `{stmt; stmt; expr; expr}`
}

impl Expr {
    pub fn lit(x: Lit) -> Self {
        Expr { body: ExprBody::Lit(x), ty: None }
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

    fn expr(e: ExprBody) -> Self {
        Expr { body: e, ty: None }
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

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.body {
            ExprBody::Lit(a)            => write!(f, "{}", a),
            // ExprBody::Var(x)            => write!(f, "{}", x),
            ExprBody::Var(x)            => write!(f, "{}", x.pretty()),
            ExprBody::Abs(x, e)         => write!(f, "λ{}.{}", x, e),
            ExprBody::App(e1, e2) => {
                if let ExprBody::App(_, _) = e2.body {
                    write!(f, "{} ({})", e1, e2)
                }
                else {
                    write!(f, "{} {}", e1, e2)
                }
            }
            ExprBody::If(e1, e2, e3)    => write!(f, "if ({}) {} else {}", e1, e2, e3),
            ExprBody::Match(expr, arms) => {
                let s: Vec<String>
                    = arms.iter()
                          .map(|(p, e)| format!("{} => {},", p, e))
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
            ExprBody::FieldAccess(base, field) => {
                match base.body {
                    ExprBody::Abs(_,_) | ExprBody::App(_,_) | ExprBody::If(_,_,_) => {
                        write!(f, "({}).{}", base, field)
                    }
                    _ => write!(f, "{}.{}", base, field)
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
                let s: Vec<_> = items.iter().map(|e| format!("{}", e)).collect();
                write!(f, "{{{}}}", s.join("; "))
            }
        }
    }
}
