use indexmap::IndexMap;
use crate::error::*;
use crate::module::*;
use crate::syntax::ast::*;
use crate::runtime::*;

#[derive(Clone, Debug)]
pub struct CoreIR {
    pub globals: GlobalEnv,
    pub expressions: Vec<CoreExpr>,
}

pub type GlobalEnv = IndexMap<Symbol, CoreExpr>;

pub type Label = String;        // label of record field or region

#[derive(Clone, Debug, PartialEq)]
pub enum CoreExpr {
    // functions
    Lam(Pat, Box<CoreExpr>),    // lambda
    Builtin(Builtin),           // builtin functions

    // expressions
    GlobalVar(Symbol),                                // global variables
    Var(Symbol),                                      // local variables
    App(Box<CoreExpr>, Box<CoreExpr>),                // strict App `f x`
    Match(Box<CoreExpr>, Vec<(Pat, CoreExpr)>),       // `match (e) { p1 => e1, ... }`
    For(Box<CoreExpr>, Box<CoreExpr>, Box<CoreExpr>), // `__for__ (init; pred; next)`
    TupleAccess(Box<CoreExpr>, usize),                // ex. `p.0`
    FieldAccess(Box<CoreExpr>, Label),                // ex. `p.x`

    Let(Pat, Box<CoreExpr>, Box<CoreExpr>),       // [local] `let p = x in e`
    LetRec(Symbol, Box<CoreExpr>, Box<CoreExpr>), // [local] `let rec f = x in e`

    // values (saturated expressions)
    Lit(Lit),
    Con(Symbol, Vec<CoreExpr>),     // `Con "Cons" [x, xs]`, `Con "Nil" []`
    Tuple(Vec<CoreExpr>),           // `Tuple [e1, e2, ...]`
    Record(Vec<(Label, CoreExpr)>), // `Record [("x", e1), ("y", e2), ...]`
}

impl CoreExpr {
    pub fn local_var<S: Into<String>>(s: S) -> Self {
        CoreExpr::Var(Symbol::local(s))
    }

    // ---------------------------------------------------------
    pub fn lam(p: Pat, e: CoreExpr) -> CoreExpr {
        CoreExpr::Lam(p, Box::new(e))
    }
    pub fn app(f: CoreExpr, x: CoreExpr) -> CoreExpr {
        CoreExpr::App(Box::new(f), Box::new(x))
    }
    pub fn if_(c: CoreExpr, t: CoreExpr, f: CoreExpr) -> CoreExpr {
        CoreExpr::match_(c, vec![
            (Pat::Lit(Lit::Bool(true)), t),
            (Pat::Wildcard, f),
        ])
    }
    pub fn match_(scrut: CoreExpr, arms: Vec<(Pat, CoreExpr)>) -> CoreExpr {
        CoreExpr::Match(Box::new(scrut), arms)
    }
    pub fn for_(init: CoreExpr, pred: CoreExpr, next: CoreExpr) -> CoreExpr {
        CoreExpr::For(Box::new(init), Box::new(pred), Box::new(next))
    }
    pub fn tuple_access(t: CoreExpr, index: usize) -> CoreExpr {
        CoreExpr::TupleAccess(Box::new(t), index)
    }
    pub fn field_access(r: CoreExpr, label: Label) -> CoreExpr {
        CoreExpr::FieldAccess(Box::new(r), label)
    }

    pub fn let_(p: Pat, x: CoreExpr, e: CoreExpr) -> CoreExpr {
        CoreExpr::Let(p, Box::new(x), Box::new(e))
    }
    pub fn letrec(f: Symbol, x: CoreExpr, e: CoreExpr) -> CoreExpr {
        CoreExpr::LetRec(f, Box::new(x), Box::new(e))
    }

    // ---------------------------------------------------------
    pub fn unit() -> CoreExpr {
        CoreExpr::Lit(Lit::Unit)
    }
    pub fn bool_(x: bool) -> CoreExpr {
        CoreExpr::Lit(Lit::Bool(x))
    }
    pub fn int(x: i64) -> CoreExpr {
        CoreExpr::Lit(Lit::Int(x))
    }
}

// -------------------------------------------------------------
impl CoreIR {
    /// Construct CoreIR with initial globals.
    pub fn new() -> CoreIR {
        CoreIR { globals: globals(), expressions: Vec::new() }
    }

    /// Convert top-level (global) AST items to `CoreExpr` and update `CoreIR`
    /// with them.
    pub fn lower(&mut self, items: &[Item]) -> Result<(), Error> {
        for item in items {
            self.lower_global_item(item)?;
        }
        Ok(())
    }
}

// -------------------------------------------------------------
impl CoreIR {
    fn lower_global_item(&mut self, item: &Item) -> Result<(), Error> {
        match &item.body {
            ItemBody::Decl(decl) => {
                match decl {
                    // Register constructor function for each variants.
                    Decl::NamedType(typedef) => {
                        for variant in &typedef.variants {
                            let e = make_con(variant.name(), variant.arity());
                            self.globals.insert(variant.name(), e);
                        }
                    }
                    _ => {}
                }
            }
            ItemBody::Stmt(stmt) => {
                match stmt {
                    // Register top-level functions or variables.
                    Stmt::Let(p, e) | Stmt::LetRec(p, e) => {
                        let Pat::Var(sym@Symbol::Unique(_)) = p else { unreachable!("non-global var pattern: {:?}", p) };
                        let e = lower_expr(e)?;
                        self.globals.insert(sym.clone(), e);
                    }
                }
            }
            ItemBody::Expr(expr) => {
                let e = lower_expr(expr)?;
                self.expressions.push(e);
            }
        }
        Ok(())
    }
}

// -------------------------------------------------------------
/// Make constructor function as CoreExpr.
fn make_con(name: Symbol, arity: usize) -> CoreExpr {
    let mut args = Vec::with_capacity(arity);
    let mut vars = Vec::with_capacity(arity);
    for i in 0..arity {
        let v = format!("a{}", i);
        args.push(v.clone());
        vars.push(CoreExpr::local_var(v));
    }
    let mut e = CoreExpr::Con(name, vars);
    for v in args.into_iter().rev() {
        e = CoreExpr::lam(Pat::local_var(v), e)
    }
    e
}

// -------------------------------------------------------------
/// Construct global or local symbol.
fn symbol<S: Into<String>>(s: S) -> Symbol {
    let s = s.into();
    if s.starts_with("::") {
        let xs: Vec<_> = s.split("::").collect();
        Symbol::Unique(Path::absolute(xs[1..].to_vec()))
    }
    else {
        Symbol::local(s)
    }
}

// -------------------------------------------------------------
/// Construct initial GlobalEnv.
fn globals() -> GlobalEnv {
    fn add_op(global: &mut GlobalEnv, prim_sym: &str, prim: Builtin) {
        global.insert(
            symbol(prim_sym),
            CoreExpr::Builtin(prim),
        );
    }

    let mut global = GlobalEnv::new();

    add_op(&mut global, "::core::__i64_neg__", Builtin::I64Neg);

    add_op(&mut global, "::core::__i64_add__", Builtin::I64Add);
    add_op(&mut global, "::core::__i64_sub__", Builtin::I64Sub);
    add_op(&mut global, "::core::__i64_mul__", Builtin::I64Mul);
    add_op(&mut global, "::core::__i64_div__", Builtin::I64Div);
    add_op(&mut global, "::core::__i64_mod__", Builtin::I64Mod);

    add_op(&mut global, "::core::__i64_eq__", Builtin::I64Eq);
    add_op(&mut global, "::core::__i64_ne__", Builtin::I64Neq);

    add_op(&mut global, "::core::__i64_le__", Builtin::I64Le);
    add_op(&mut global, "::core::__i64_lt__", Builtin::I64Lt);
    add_op(&mut global, "::core::__i64_ge__", Builtin::I64Ge);
    add_op(&mut global, "::core::__i64_gt__", Builtin::I64Gt);

    global
}

// -------------------------------------------------------------
fn lower_local_items(items: &[Item]) -> Result<CoreExpr, Error> {
    match items {
        [] => Ok(CoreExpr::unit()),
        [head, tail@..] => match &head.body {
            ItemBody::Decl(_) => unreachable!(),
            ItemBody::Stmt(stmt) => match stmt {
                Stmt::Let(p, x) => {
                    let x = lower_expr(x)?;
                    let e = lower_local_items(tail)?;
                    Ok(CoreExpr::let_(p.clone(), x, e))
                }
                Stmt::LetRec(p, x) => {
                    let Pat::Var(sym@Symbol::Local(_)) = p else { unreachable!("non-local var pattern: {:?}", p) };
                    let x = lower_expr(x)?;
                    let e = lower_local_items(tail)?;
                    Ok(CoreExpr::letrec(sym.clone(), x, e))
                }
            },
            ItemBody::Expr(expr) => match tail {
                [] => lower_expr(expr),
                _ => {
                    let x = lower_expr(expr)?;
                    let e = lower_local_items(tail)?;
                    Ok(CoreExpr::let_(Pat::Wildcard, x, e))
                }
            },
        },
    }
}

fn lower_expr(expr: &Expr) -> Result<CoreExpr, Error> {
    match &expr.body {
        ExprBody::Var(sym) => {
            match sym {
                Symbol::Local(_) => Ok(CoreExpr::Var(sym.clone())),
                Symbol::Unique(_) => Ok(CoreExpr::GlobalVar(sym.clone())),
                Symbol::Unresolved(_) => Err(Error::UnboundVariable(sym.clone())),
            }
        }
        ExprBody::Abs(p, e) => {
            let e = lower_expr(e)?;
            Ok(CoreExpr::lam(p.clone(), e))
        }
        ExprBody::App(f, x) => {
            let f = lower_expr(f)?;
            let x = lower_expr(x)?;
            Ok(CoreExpr::app(f, x))
        }
        ExprBody::Block(items) => {
            lower_local_items(items)
        }
        ExprBody::Builtin(b) => {
            Ok(CoreExpr::Builtin(b.clone()))
        }
        ExprBody::For(init, pred, next) => {
            let init = lower_expr(init)?;
            let pred = lower_expr(pred)?;
            let next = lower_expr(next)?;
            Ok(CoreExpr::for_(init, pred, next))
        }
        ExprBody::If(c, t, f) => {
            let c = lower_expr(c)?;
            let t = lower_expr(t)?;
            let f = lower_expr(f)?;
            Ok(CoreExpr::if_(c, t, f))
        }
        ExprBody::Match(scrut, arms) => {
            let scrut = lower_expr(scrut)?;
            let mut xs = Vec::new();
            for (p, e) in arms.iter() {
                let e = lower_expr(e)?;
                xs.push((p.clone(), e))
            }
            Ok(CoreExpr::match_(scrut, xs))
        }
        ExprBody::TupleAccess(e, index) => {
            let e = lower_expr(e)?;
            Ok(CoreExpr::tuple_access(e, *index))
        }
        ExprBody::FieldAccess(e, label) => {
            let e = lower_expr(e)?;
            Ok(CoreExpr::field_access(e, label.clone()))
        }

        ExprBody::Lit(x) => {
            Ok(CoreExpr::Lit(x.clone()))
        }
        ExprBody::Con(sym, es) => {
            let mut xs = Vec::new();
            for e in es.iter() {
                let e = lower_expr(e)?;
                xs.push(e);
            }
            Ok(CoreExpr::Con(sym.clone(), xs))
        }
        ExprBody::Tuple(es) => {
            let mut xs = Vec::new();
            for e in es.iter() {
                let e = lower_expr(e)?;
                xs.push(e);
            }
            Ok(CoreExpr::Tuple(xs))
        }
        ExprBody::Record(fields) => {
            let mut xs = Vec::new();
            for (label, e) in fields.iter() {
                let e = lower_expr(e)?;
                xs.push((label.clone(), e));
            }
            Ok(CoreExpr::Record(xs))
        }

        ExprBody::RawTraitRecord(_) => unreachable!(),
        ExprBody::TraitRecord(_) => unreachable!(),
    }
}
