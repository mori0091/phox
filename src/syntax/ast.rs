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

    Tuple(Vec<Type>),
    Struct(String, Vec<(String, Type)>),
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
            Type::Tuple(ts) => {
                write!(f, "({},", ts[0])?;
                if ts.len() > 1 {
                    let s: Vec<String> = ts[1..].iter().map(|t| t.to_string()).collect();
                    write!(f, " {}", s.join(", "))?;
                }
                write!(f, ")")
            }
            Type::Struct(_, _) => todo!(),
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

// ===== Type schemes (∀ vars . ty) =====
#[derive(Clone, Debug)]
pub struct Scheme {
    pub vars: Vec<TypeVarId>, // quantified variables
    pub ty: Type,
}

impl fmt::Display for Scheme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.vars.is_empty() {
            // 量化変数がなければそのまま型のみ
            write!(f, "{}", self.ty)
        } else {
            write!(f, "∀ {}. {}", TypeVarList(&self.vars), self.ty)
        }
    }
}

struct TypeVarList<'a>(&'a [TypeVarId]);

impl<'a> fmt::Display for TypeVarList<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = self.0
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
            .join(" ");
        write!(f, "{}", s)
    }
}

impl Scheme {
    pub fn pretty(&self) -> String {
        use std::collections::HashMap;

        // 量化変数に a, b, c... を割り当てる
        let mut map = HashMap::new();
        for (i, v) in self.vars.iter().enumerate() {
            let ch = (b'a' + i as u8) as char;
            map.insert(*v, ch.to_string());
        }

        fn rename(ty: &Type, map: &HashMap<TypeVarId, String>) -> Type {
            match ty {
                Type::Var(v) => {
                    if let Some(name) = map.get(v) {
                        Type::Con(name.clone()) // ここでは Var を Con に置き換えてもよい
                    } else {
                        Type::Var(*v) // 自由変数はそのまま
                    }
                }
                Type::Con(name) => Type::Con(name.clone()),
                Type::Fun(t1, t2) => {
                    Type::fun(rename(t1, map), rename(t2, map))
                }
                Type::App(t1, t2) => {
                    Type::app(rename(t1, map), rename(t2, map))
                }
                Type::Tuple(ts) => {
                    Type::Tuple(ts.iter().map(|t| rename(t, map)).collect())
                }
                Type::Struct(_, _) => todo!(),
            }
        }

        let renamed_ty = rename(&self.ty, &map);

        if self.vars.is_empty() {
            format!("{}", renamed_ty)
        } else {
            let vars: Vec<String> = (0..self.vars.len())
                .map(|i| ((b'a' + i as u8) as char).to_string())
                .collect();
            format!("∀ {}. {}", vars.join(" "), renamed_ty)
        }
    }
}

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

#[derive(Debug)]
pub enum Expr {
    Var(String),
    Abs(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    LetRec(String, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),

    Tuple(Vec<Expr>),
    Struct(String, Vec<(String, Expr)>),

    Lit(Lit),
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
            Expr::Lit(a)            => write!(f, "{}", a),
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
