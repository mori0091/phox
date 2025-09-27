use std::fmt;

// ===== Kinds =====
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Kind {
    Star,                        // *
    Arrow(Box<Kind>, Box<Kind>), // k1 -> k2
}

// ===== Types =====
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Var(TypeVarId),             // 型変数
    Fun(Box<Type>, Box<Type>),  // 関数型
    Con(String),                // 型構築子
    App(Box<Type>, Box<Type>),  // 型適用
}

impl Type {
    pub fn var(id: TypeVarId) -> Self {
        Type::Var(id)
    }
    pub fn con<S: Into<String>>(s: S) -> Self {
        Type::Con(s.into())
    }
    pub fn app(f: Type, x: Type) -> Self {
        Type::App(Box::new(f), Box::new(x))
    }
    pub fn fun(a: Type, b: Type) -> Self {
        Type::Fun(Box::new(a), Box::new(b))
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Var(v) => write!(f, "{}", v),
            Type::Con(name) => write!(f, "{}", name),
            Type::Fun(a, b) => {
                // 左側は必要なら括弧
                match **a {
                    Type::Fun(_, _) => write!(f, "({})", a)?,
                    _ => write!(f, "{}", a)?,
                }
                write!(f, " -> {}", b)
            }
            Type::App(fun, arg) => {
                // fun はそのまま表示
                write!(f, "{}", fun)?;
                // arg は Con や Var ならそのまま、App や Fun なら括弧
                match **arg {
                    Type::Con(_) | Type::Var(_) => write!(f, " {}", arg),
                    _ => write!(f, " ({})", arg),
                }
            }
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct TypeVarId(pub usize);

impl fmt::Display for TypeVarId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "?{}", self.0)
    }
}

// ===== AST =====
#[derive(Debug)]
pub enum Expr {
    Var(String),
    Abs(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    LetRec(String, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    LitInt(i64),
    LitBool(bool),
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
    pub fn let_<S: Into<String>>(s: S, e1: Expr, e2: Expr) -> Self {
        Expr::Let(s.into(), Box::new(e1), Box::new(e2))
    }
    pub fn let_rec<S: Into<String>>(s: S, e1: Expr, e2: Expr) -> Self {
        Expr::LetRec(s.into(), Box::new(e1), Box::new(e2))
    }
    pub fn if_(e1: Expr, e2: Expr, e3: Expr) -> Self {
        Expr::If(Box::new(e1), Box::new(e2), Box::new(e3))
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Var(x)            => write!(f, "{}", x),
            Expr::Abs(x, e)         => write!(f, "λ{}.{}", x, *e),
            Expr::App(e1, e2)       => write!(f, "{} {}", *e1, *e2),
            Expr::Let(x, e1, e2)    => write!(f, "let {} = {} in {}", x, *e1, *e2),
            Expr::LetRec(x, e1, e2) => write!(f, "let rec {} = {} in {}", x, *e1, *e2),
            Expr::If(e1, e2, e3)    => write!(f, "if {} then {} else {}", e1, e2, e3),
            Expr::LitInt(a)         => write!(f, "{}", a),
            Expr::LitBool(a)        => write!(f, "{}", a),
        }
    }
}
