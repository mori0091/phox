use std::fmt;

// ===== AST =====
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Lit {
    Unit,        // ()
    Bool(bool),  // true / false
    Int(i64),    // 整数リテラル
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lit::Unit => write!(f, "()"),
            Lit::Bool(b) => write!(f, "{}", b),
            Lit::Int(i) => write!(f, "{}", i),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Pat {
    Lit(Lit),                   // `()`, `true`, `1`, etc.
    Var(String),                // `x`
    Wildcard,                   // `_`
    Con(String, Vec<Pat>),      // `Cons x xs`
    Tuple(Vec<Pat>),            // `(p,)`, `(p1, p2)`
    Struct(String, Vec<(String, Pat)>),
}

impl Pat {
    pub fn var<S: Into<String>>(s: S) -> Self {
        Pat::Var(s.into())
    }
    pub fn con<S: Into<String>>(s: S, args: Vec<Pat>) -> Self {
        Pat::Con(s.into(), args)
    }
}

impl fmt::Display for Pat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pat::Lit(a) => write!(f, "{}", a),
            Pat::Var(x) => write!(f, "{}", x),
            Pat::Wildcard => write!(f, "_"),
            Pat::Con(name, args) => {
                if args.is_empty() {
                    write!(f, "{}", name)
                }
                else {
                    write!(f, "{}", name)?;
                    let s: Vec<String> = args.iter().map(|arg| arg.to_string()).collect();
                    write!(f, " {}", s.join(" "))
                }
            }
            Pat::Tuple(es) => {
                write!(f, "({},", es[0])?;
                if es.len() > 1 {
                    let s: Vec<String> = es[1..].iter().map(|t| t.to_string()).collect();
                    write!(f, " {}", s.join(", "))?;
                }
                write!(f, ")")
            }
            Pat::Struct(_, _) => todo!(),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Lit(Lit),
    Var(String),
    Abs(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Let(Pat, Box<Expr>, Box<Expr>),
    LetRec(Pat, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),

    Tuple(Vec<Expr>),
    Struct(String, Vec<(String, Expr)>),
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
    pub fn let_(p: Pat, e1: Expr, e2: Expr) -> Self {
        Expr::Let(p, Box::new(e1), Box::new(e2))
    }
    pub fn let_rec(p: Pat, e1: Expr, e2: Expr) -> Self {
        Expr::LetRec(p, Box::new(e1), Box::new(e2))
    }
    pub fn if_(e1: Expr, e2: Expr, e3: Expr) -> Self {
        Expr::If(Box::new(e1), Box::new(e2), Box::new(e3))
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Lit(a)            => write!(f, "{}", a),
            Expr::Var(x)            => write!(f, "{}", x),
            Expr::Abs(x, e)         => write!(f, "λ{}.{}", x, *e),
            Expr::App(e1, e2)       => write!(f, "{} {}", *e1, *e2),
            Expr::Let(x, e1, e2)    => write!(f, "let {} = {} in {}", x, *e1, *e2),
            Expr::LetRec(x, e1, e2) => write!(f, "let rec {} = {} in {}", x, *e1, *e2),
            Expr::If(e1, e2, e3)    => write!(f, "if {} then {} else {}", e1, e2, e3),
            Expr::Tuple(es) => {
                write!(f, "({},", es[0])?;
                if es.len() > 1 {
                    let s: Vec<String> = es[1..].iter().map(|t| t.to_string()).collect();
                    write!(f, " {}", s.join(", "))?;
                }
                write!(f, ")")
            }
            Expr::Struct(_, _) => todo!(),
        }
    }
}
