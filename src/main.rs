use std::collections::{HashMap, HashSet};
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

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct TypeVarId(pub usize);

impl fmt::Display for TypeVarId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "?{}", self.0)
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

// ===== Type schemes (∀ vars . ty) =====
#[derive(Clone)]
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
            write!(f, "∀{}. {}", TypeVarList(&self.vars), self.ty)
        }
    }
}

struct TypeVarList<'a>(&'a [TypeVarId]);

impl<'a> fmt::Display for TypeVarList<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = self.0.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(" ");
        write!(f, "{}", s)
    }
}

// ===== Type Environment =====
pub struct TypeEnv {
    pub kinds: HashMap<String, Kind>, // 型構築子の種類
}

// ===== Environment =====
#[derive(Default, Clone)]
pub struct Env {
    pub map: HashMap<String, Scheme>,
}

// ===== Type context: union-find + binding =====
pub struct TypeContext {
    parent: Vec<TypeVarId>,    // union-find parent pointers
    binding: Vec<Option<Type>>, // representative binding (Some if bound to a type)
}

impl TypeContext {
    pub fn new() -> Self {
        Self { parent: Vec::new(), binding: Vec::new() }
    }

    pub fn fresh_var(&mut self) -> TypeVarId {
        let id = TypeVarId(self.parent.len());
        self.parent.push(id);
        self.binding.push(None);
        id
    }

    // find with path compression
    fn find(&mut self, id: TypeVarId) -> TypeVarId {
        let p = self.parent[id.0];
        if p != id {
            let root = self.find(p);
            self.parent[id.0] = root;
        }
        self.parent[id.0]
    }

    // Occurs check: does tv occur in ty (following bindings)?
    fn occurs_in(&mut self, tv: TypeVarId, ty: &Type) -> bool {
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
            Type::Fun(a, b) => self.occurs_in(tv, a) || self.occurs_in(tv, b),
            _ => false,
        }
    }

    // Normalize a type by chasing bindings and compressing vars
    pub fn repr(&mut self, ty: &Type) -> Type {
        match ty {
            Type::Var(v) => {
                let r = self.find(*v);
                // ここでも Option<Type> をクローンして借用を解放
                let bound_opt = self.binding[r.0].clone();
                if let Some(bound) = bound_opt {
                    self.repr(&bound)
                } else {
                    Type::Var(r)
                }
            }
            Type::Fun(a, b) => Type::fun(self.repr(a), self.repr(b)),
            Type::App(f, x) => Type::app(self.repr(f), self.repr(x)),
            Type::Con(c) => Type::con(c.clone()),
        }
    }

    pub fn unify(&mut self, a: &Type, b: &Type) -> Result<(), String> {
        let a = self.repr(a);
        let b = self.repr(b);
        match (&a, &b) {
            (Type::Var(v1), Type::Var(v2)) => {
                let r1 = self.find(*v1);
                let r2 = self.find(*v2);
                if r1 != r2 {
                    // union by attaching r1 under r2
                    self.parent[r1.0] = r2;
                }
                Ok(())
            }

            (Type::Var(v), t) | (t, Type::Var(v)) => {
                let r = self.find(*v);
                // t might reduce further; use repr to avoid deep chains
                let t = self.repr(t);
                if self.occurs_in(r, &t) {
                    Err("occurs check failed: recursive type".into())
                } else {
                    self.binding[r.0] = Some(t);
                    Ok(())
                }
            }

            (Type::Fun(a1, b1), Type::Fun(a2, b2)) => {
                self.unify(a1, a2)?;
                self.unify(b1, b2)
            }

            (Type::Con(c1), Type::Con(c2)) if c1 == c2 => Ok(()),

            (Type::App(f1, x1), Type::App(f2, x2)) => {
                self.unify(f1, f2)?;
                self.unify(x1, x2)
            }

            _ => Err(format!("type mismatch: {} vs {}", a, b)),
        }
    }
}

// ===== Free type variables =====
fn free_ty_vars(ctx: &mut TypeContext, ty: &Type, acc: &mut HashSet<TypeVarId>) {
    match ctx.repr(ty) {
        Type::Var(v) => {
            acc.insert(v);
        }
        Type::Fun(ref a, ref b) => {
            free_ty_vars(ctx, a, acc);
            free_ty_vars(ctx, b, acc);
        }
        Type::Con(_) => {}
        Type::App(ref a, ref b) => {
            free_ty_vars(ctx, a, acc);
            free_ty_vars(ctx, b, acc);
        }
    }
}

fn free_env_vars(ctx: &mut TypeContext, env: &Env) -> HashSet<TypeVarId> {
    let mut acc = HashSet::new();
    for scheme in env.map.values() {
        free_ty_vars(ctx, &scheme.ty, &mut acc);
        for v in &scheme.vars {
            acc.remove(v);
        }
    }
    acc
}

// ===== Instantiate / Generalize =====
fn instantiate(ctx: &mut TypeContext, sch: &Scheme) -> Type {
    let mut subst: HashMap<TypeVarId, TypeVarId> = HashMap::new();
    for &v in &sch.vars {
        subst.insert(v, ctx.fresh_var());
    }
    fn inst(ctx: &mut TypeContext, t: &Type, s: &HashMap<TypeVarId, TypeVarId>) -> Type {
        match ctx.repr(t) {
            Type::Var(v) => {
                if let Some(nv) = s.get(&v) {
                    Type::Var(*nv)
                } else {
                    Type::Var(v)
                }
            }
            Type::Fun(ref a, ref b) => Type::fun(inst(ctx, a, s), inst(ctx, b, s)),
            Type::Con(c)            => Type::con(c),
            Type::App(ref a, ref b) => Type::app(inst(ctx, a, s), inst(ctx, b, s)),
        }
    }
    inst(ctx, &sch.ty, &subst)
}

fn generalize(ctx: &mut TypeContext, env: &Env, ty: &Type) -> Scheme {
    let mut fty = HashSet::new();
    free_ty_vars(ctx, ty, &mut fty);
    let fenv = free_env_vars(ctx, env);
    let mut vars: Vec<TypeVarId> = fty.difference(&fenv).cloned().collect();
    vars.sort_by_key(|v| v.0);
    Scheme { vars, ty: ctx.repr(ty) }
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

// ===== Inference (Algorithm J core) =====
pub fn infer(ctx: &mut TypeContext, env: &mut Env, expr: &Expr) -> Result<Type, String> {
    match expr {
        Expr::Var(x) => {
            let sch = env.map.get(x).ok_or_else(|| format!("unbound variable: {}", x))?;
            Ok(instantiate(ctx, sch))
        }
        Expr::Abs(x, body) => {
            // introduce parameter with fresh monomorphic type variable
            let tv = Type::Var(ctx.fresh_var());
            // extend env temporarily
            let saved = env.clone();
            env.map.insert(x.clone(), Scheme { vars: vec![], ty: tv.clone() });
            let tbody = infer(ctx, env, body)?;
            *env = saved;
            Ok(Type::fun(tv, tbody))
        }
        Expr::App(f, a) => {
            let tf = infer(ctx, env, f)?;
            let ta = infer(ctx, env, a)?;
            let tr = Type::Var(ctx.fresh_var()); // result type variable
            ctx.unify(&tf, &Type::fun(ta, tr.clone()))?;
            Ok(tr)
        }
        Expr::Let(x, e1, e2) => {
            let t1 = infer(ctx, env, e1)?;
            let sch = generalize(ctx, env, &t1);
            env.map.insert(x.clone(), sch);
            infer(ctx, env, e2)
        }
        Expr::LetRec(x, e1, e2) => {
            // 1. fresh type variable
            let tv = Type::Var(ctx.fresh_var());
            // 2. 環境に仮の型を追加
            env.map.insert(x.clone(), Scheme { vars: vec![], ty: tv.clone() });
            // 3. e1 の型を推論
            let t1 = infer(ctx, env, e1)?;
            // 4. unify で仮の型と一致させる
            ctx.unify(&tv, &t1)?;
            // 5. 一般化して環境に登録し直す
            let sch = generalize(ctx, env, &t1);
            env.map.insert(x.clone(), sch);
            // 6. e2 の型を推論
            infer(ctx, env, e2)
        }
        Expr::If(cond, then_e, else_e) => {
            let t_cond = infer(ctx, env, cond)?;
            // ctx.unify(&t_cond, &Type::Bool)?;
            ctx.unify(&t_cond, &Type::Con("Bool".into()))?;

            let t_then = infer(ctx, env, then_e)?;
            let t_else = infer(ctx, env, else_e)?;
            ctx.unify(&t_then, &t_else)?;
            Ok(t_then)
        }

        Expr::LitInt(_) => Ok(Type::Con("Int".into())),
        Expr::LitBool(_) => Ok(Type::Con("Bool".into())),
    }
}

// ------------------------
fn initial_type_env() -> TypeEnv {
    let mut tenv = TypeEnv { kinds: HashMap::new() };

    tenv.kinds.insert("Int".into(), Kind::Star);
    tenv.kinds.insert("Bool".into(), Kind::Star);
    tenv.kinds.insert("List".into(), Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star)));
    tenv.kinds.insert("Option".into(), Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star)));

    tenv
}

fn initial_env(ctx: &mut TypeContext) -> Env {
    let mut env = Env::default();

    // None : ∀a. Option a
    let a = ctx.fresh_var();
    env.map.insert(
        "None".into(),
        Scheme {
            vars: vec![a],
            ty: Type::app(
                Type::con("Option"),
                Type::var(a),
            ),
        },
    );

    // Some : ∀a. a -> Option a
    let a = ctx.fresh_var();
    env.map.insert(
        "Some".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(
                Type::var(a),
                Type::app(
                    Type::con("Option"),
                    Type::var(a),
                ),
            ),
        },
    );

    // Nil : ∀a. List a
    let a = ctx.fresh_var();
    env.map.insert(
        "Nil".into(),
        Scheme {
            vars: vec![a],
            ty: Type::app(Type::con("List"), Type::var(a)),
        },
    );

    // Cons : ∀a. a -> List a -> List a
    let a = ctx.fresh_var();
    env.map.insert(
        "Cons".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(
                Type::var(a),
                Type::fun(
                    Type::app(Type::con("List"), Type::var(a)),
                    Type::app(Type::con("List"), Type::var(a)),
                ),
            ),
        },
    );

    // map : ∀a b. (a -> b) -> List a -> List b
    let a = ctx.fresh_var();
    let b = ctx.fresh_var();

    env.map.insert(
        "map".into(),
        Scheme {
            vars: vec![a, b],
            ty: Type::fun(
                Type::fun(Type::var(a), Type::var(b)), // (a -> b)
                Type::fun(
                    Type::app(Type::con("List"), Type::var(a)), // List a
                    Type::app(Type::con("List"), Type::var(b)), // List b
                ),
            ),
        },
    );

    // プリミティブ演算子 (+, -, *, ==) も必要なら追加
    let a = ctx.fresh_var();
    env.map.insert(
        "+".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::var(a))),
        },
    );

    let a = ctx.fresh_var();
    env.map.insert(
        "-".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::var(a))),
        },
    );

    let a = ctx.fresh_var();
    env.map.insert(
        "*".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::var(a))),
        },
    );

    let a = ctx.fresh_var();
    env.map.insert(
        "==".into(),
        Scheme {
            vars: vec![a],
            ty: Type::fun(Type::var(a), Type::fun(Type::var(a), Type::con("Bool"))),
        },
    );

    env
}

// ===== Demo: pretty-print principal types =====
fn main() {
    let tests = testcases();
    for (name, expr) in tests {
        println!("{}: {}", name, expr);
        let mut _tenv = initial_type_env();
        let mut ctx = TypeContext::new();
        let mut env = initial_env(&mut ctx);
        match infer(&mut ctx, &mut env, &expr) {
            Ok(ty) => {
                let sch = generalize(&mut ctx, &env, &ty);
                println!(" : {}", sch);
            }
            Err(e) => {
                println!(" : type error: {}", e);
            }
        }
    }
}

fn testcases() -> Vec<(&'static str, Expr)> {
    // 定数関数 K
    // let k = \x. \y. x in k
    let expr_k = Expr::let_(
        "k",
        Expr::abs("x", Expr::abs("y", Expr::var("x"))),
        Expr::var("k"),
    );

    // S コンビネータ
    // \x. \y. \z. x z (y z)
    let expr_s = Expr::abs(
        "x",
        Expr::abs(
            "y",
            Expr::abs(
                "z",
                Expr::app(
                    Expr::app(
                        Expr::var("x"),
                        Expr::var("z")
                    ),
                    Expr::app(
                        Expr::var("y"),
                        Expr::var("z")
                    )
                )
            )
        )
    );

    // 関数合成 (B コンビネータ)
    // \f. \g. \x. f (g x)
    let expr_b = Expr::abs(
        "f",
        Expr::abs(
            "g",
            Expr::abs(
                "x",
                Expr::app(
                    Expr::var("f"),
                    Expr::app(
                        Expr::var("g"),
                        Expr::var("x")
                    )
                )
            )
        )
    );

    // 二重適用
    // \f. \x. f (f x)
    let expr_double = Expr::abs(
        "f",
        Expr::abs(
            "x",
            Expr::app(
                Expr::var("f"),
                Expr::app(
                    Expr::var("f"),
                    Expr::var("x")
                )
            )
        )
    );

    // ----------------------------
    // 階乗関数(factorial)
    // let rec fact = \n.
    //     if (n == 0) then 1 else n * (fact (n - 1))
    // in fact
    let fact_expr = Expr::let_rec(
        "fact",
        Expr::abs(
            "n",
            Expr::if_(
                Expr::app(
                    Expr::app(
                        Expr::var("=="),
                        Expr::var("n")
                    ),
                    Expr::LitInt(0)
                ),
                Expr::LitInt(1),
                Expr::app(
                    Expr::app(
                        Expr::var("*"),
                        Expr::var("n")
                    ),
                    Expr::app(
                        Expr::var("fact"),
                        Expr::app(
                            Expr::app(
                                Expr::var("-"), // ← 引き算もプリミティブに追加
                                Expr::var("n")
                            ),
                            Expr::LitInt(1)
                        )
                    )
                )
            )
        ),
        Expr::var("fact"),
    );

    // ----------------------------
    // 無限リスト(ones)
    // let rec ones = 1 :: ones in ones
    let ones_expr = Expr::let_rec(
        "ones",
        Expr::app(
            Expr::app(
                Expr::var("Cons"),
                Expr::LitInt(1),
            ),
            Expr::var("ones"),
        ),
        Expr::var("ones"),
    );

    // ----------------------------
    // 再帰的恒等関数(自己参照)
    // let rec id = \x. id x in id
    let rec_id_expr = Expr::let_rec(
        "id",
        Expr::abs("x",
                  Expr::app(Expr::var("id"),
                            Expr::var("x"))),
        Expr::var("id"),
    );

    // ----------------------------
    // map Some [1,2,3]
    let map_some = Expr::app(
        Expr::app(
            Expr::var("map"),
            Expr::var("Some")
        ),
        Expr::app(
            Expr::app(
                Expr::var("Cons"),
                Expr::LitInt(1),
            ),
            Expr::app(
                Expr::app(
                    Expr::var("Cons"),
                    Expr::LitInt(2),
                ),
                Expr::app(
                    Expr::app(
                        Expr::var("Cons"),
                        Expr::LitInt(3),
                    ),
                    Expr::var("Nil"),
                ),
            ),
        ),
    );

    // ----------------------------
    let tests = vec![
        ("id-id", Expr::let_(
            "id",
            Expr::abs("x", Expr::var("x")),
            Expr::app(
                Expr::var("id"),
                Expr::var("id"))
        )),
        ("omega", Expr::abs(
            "x",
            Expr::app(Expr::var("x"), Expr::var("x"))
        )),
        ("k", expr_k),
        ("s", expr_s),
        ("b", expr_b),
        ("double", expr_double),

        // 再帰関数
        ("fact", fact_expr),
        ("ones", ones_expr),
        ("rec_id", rec_id_expr),

        // データ構築子; Option a
        ("None", Expr::var("None")),
        ("Some", Expr::var("Some")),
        ("Some 1", Expr::app(
            Expr::var("Some"),
            Expr::LitInt(1),
        )),
        // データ構築子; List a
        ("Nil", Expr::var("Nil")),
        ("Cons", Expr::var("Cons")),
        ("Cons 1 Nil", Expr::app(
            Expr::app(
                Expr::var("Cons"),
                Expr::LitInt(1)
            ),
            Expr::var("Nil")
        )),

        ("map Some [1,2,3]", map_some),
    ];

    tests
}
