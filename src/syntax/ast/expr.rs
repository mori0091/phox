use std::fmt;
use super::{Item, Lit, Pat};

#[derive(Clone, Debug)]
pub enum Expr {
    Lit(Lit),
    Var(String),
    App(Box<Expr>, Box<Expr>),

    // /// `let p = e1 in e2` ; binds and evaluate in local scope
    // Let(Pat, Box<Expr>, Box<Expr>),
    // /// `let rec p = e1 in e2`
    // LetRec(Pat, Box<Expr>, Box<Expr>),
    Abs(String, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Vec<(Pat, Expr)>),

    Tuple(Vec<Expr>),               // ex. `(1,)`, `(1, true, ())`
    Record(Vec<(String, Expr)>),    // ex. `@{ x:a, y:b }`
    FieldAccess(Box<Expr>, String), // ex. `p.x`
    TupleAccess(Box<Expr>, usize),  // ex. `p.0`
    Block(Vec<Item>),               // ex. `{stmt; stmt; expr; expr}`
}

impl Expr {
    pub fn var<S: Into<String>>(s: S) -> Self {
        Expr::Var(s.into())
    }
    pub fn app(f: Expr, x: Expr) -> Self {
        Expr::App(Box::new(f), Box::new(x))
    }
    // pub fn let_(p: Pat, e1: Expr, e2: Expr) -> Self {
    //     Expr::Let(p, Box::new(e1), Box::new(e2))
    // }
    // pub fn let_rec(p: Pat, e1: Expr, e2: Expr) -> Self {
    //     Expr::LetRec(p, Box::new(e1), Box::new(e2))
    // }
    pub fn abs<S: Into<String>>(s: S, e: Expr) -> Self {
        Expr::Abs(s.into(), Box::new(e))
    }
    pub fn if_(e1: Expr, e2: Expr, e3: Expr) -> Self {
        Expr::If(Box::new(e1), Box::new(e2), Box::new(e3))
    }
    pub fn match_(e: Expr, arms: Vec<(Pat, Expr)>) -> Self {
        Expr::Match(Box::new(e), arms)
    }

    pub fn field_access<S: Into<String>>(e: Expr, s: S) -> Self {
        Expr::FieldAccess(Box::new(e), s.into())
    }
    pub fn tuple_access(e: Expr, index: usize) -> Self {
        Expr::TupleAccess(Box::new(e), index)
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Lit(a)            => write!(f, "{}", a),
            Expr::Var(x)            => write!(f, "{}", x),
            Expr::Abs(x, e)         => write!(f, "λ{}.{}", x, e),
            Expr::App(e1, e2) => {
                if let Expr::App(_, _) = **e2 {
                    write!(f, "{} ({})", e1, e2)
                }
                else {
                    write!(f, "{} {}", e1, e2)
                }
            }
            // Expr::Let(x, e1, e2)    => write!(f, "let {} = {} ; {}", x, e1, e2),
            // Expr::LetRec(x, e1, e2) => write!(f, "let rec {} = {} ; {}", x, e1, e2),
            Expr::If(e1, e2, e3)    => write!(f, "if ({}) {} else {}", e1, e2, e3),
            Expr::Match(expr, arms) => {
                let s: Vec<String>
                    = arms.iter()
                          .map(|(p, e)| format!("{} => {},", p, e))
                          .collect();
                write!(f, "match ({}) {{\n{}\n}}", *expr, s.join("\n"))
            }
            Expr::Tuple(es) => {
                assert!(!es.is_empty());
                if es.len() == 1 {
                    write!(f, "({},)", es[0])
                }
                else {
                    let s: Vec<String> = es.iter().map(|t| t.to_string()).collect();
                    write!(f, "({})", s.join(", "))
                }
            }
            Expr::Record(fields) => {
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
            Expr::FieldAccess(base, field) => {
                match **base {
                    Expr::Abs(_,_) | Expr::App(_,_) | Expr::If(_,_,_) => {
                        write!(f, "({}).{}", base, field)
                    }
                    _ => write!(f, "{}.{}", base, field)
                }
            }
            Expr::TupleAccess(base, index) => {
                match **base {
                    Expr::Abs(_,_) | Expr::App(_,_) | Expr::If(_,_,_) => {
                        write!(f, "({}).{}", base, index)
                    }
                    _ => write!(f, "{}.{}", base, index)
                }
            }
            Expr::Block(items) => {
                let s: Vec<_> = items.iter().map(|e| format!("{}", e)).collect();
                write!(f, "{{{}}}", s.join("; "))
            }
        }
    }
}
