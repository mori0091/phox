use std::fmt;
use super::{Item, Lit, Pat};

#[derive(Clone, Debug)]
pub enum Expr {
    Lit(Lit),
    Var(String),
    Abs(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),

    // /// `let p = e1 in e2` ; binds and evaluate in local scope
    // Let(Pat, Box<Expr>, Box<Expr>),
    // /// `let rec p = e1 in e2`
    // LetRec(Pat, Box<Expr>, Box<Expr>),

    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Vec<(Pat, Expr)>),

    Tuple(Vec<Expr>),
    Record(Vec<(String, Expr)>),

    Block(Vec<Item>),
}

impl Expr {
    pub fn var<S: Into<String>>(s: S) -> Self {
        Expr::Var(s.into())
    }
    pub fn abs<S: Into<String>>(s: S, e: Expr) -> Self {
        Expr::Abs(s.into(), Box::new(e))
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
    pub fn if_(e1: Expr, e2: Expr, e3: Expr) -> Self {
        Expr::If(Box::new(e1), Box::new(e2), Box::new(e3))
    }
    pub fn match_(e: Expr, arms: Vec<(Pat, Expr)>) -> Self {
        Expr::Match(Box::new(e), arms)
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
            Expr::Block(_items) => {
                todo!()
            }
        }
    }
}
