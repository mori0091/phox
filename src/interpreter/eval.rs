use crate::syntax::ast::{Expr, ExprBody, Item, Stmt};
use super::{Value, Env, Binding};

pub fn eval_item(item: &Item, env: &mut Env) -> Value {
    match item {
        Item::Stmt(stmt) => {
            eval_stmt(stmt, env);
            Value::Lit(Lit::Unit)
        }
        Item::Expr(expr) => {
            eval_expr(expr, env)
        }
        _ => {
            // ignore
            Value::Lit(Lit::Unit)
        }
    }
}

pub fn eval_stmt(stmt: &Stmt, env: &mut Env) {
    match stmt {
        Stmt::Let(pat, expr) => {
            let val = eval_expr(expr, env);
            if let Some(bindings) = match_pat(pat, &val) {
                env.extend(&bindings);
            } else {
                panic!("pattern match failed in let");
            }
        }
        Stmt::LetRec(pat, expr) => {
            match pat {
                Pat::Var(x) => {
                    env.insert(x.clone(), Value::Builtin(Rc::new(|_| {
                        panic!("recursive value used before initialization")
                    })));
                    let val = eval_expr(expr, env);
                    env.insert(x.clone(), val);
                }
                _ => panic!("let rec pattern not supported (only variable)"),
            }
        }
    }
}

/// 評価関数
pub fn eval_expr(expr: &Expr, env: &Env) -> Value {
    match &expr.body {
        // リテラル
        ExprBody::Lit(lit) => Value::Lit(lit.clone()),

        // 変数参照
        ExprBody::Var(name) => {
            env.get(&name)
               .unwrap_or_else(|| {
                   if let Some(ty) = &expr.ty {
                       use crate::typesys::TypeScheme;
                       let sch = TypeScheme::mono(ty.clone());
                       panic!("unbound variable: {}: {}", name, sch.pretty())
                   } else {
                       panic!("unbound variable: {}", name)
                   }
               })
        }

        // λ抽象
        ExprBody::Abs(pat, body) => {
            Value::Closure {
                pat: pat.clone(),
                body: body.clone(),
                env: env.clone(), // クロージャは定義時の環境をキャプチャ（Rc<RefCell<_>>共有）
            }
        }

        // 関数適用
        ExprBody::App(f, arg) => {
            let f_val = eval_expr(&f, env);
            let arg_val = eval_expr(&arg, env);
            match f_val {
                // ユーザ定義関数（Closure）
                Value::Closure { pat, body, env: closure_env } => {
                    if let Some(bindings) = match_pat(&pat, &arg_val) {
                        let env2 = closure_env.duplicate();
                        env2.extend(&bindings);
                        eval_expr(&body, &env2)
                    } else {
                        panic!("function argument pattern match failed");
                    }
                }

                // 組み込み関数（Builtin）
                Value::Builtin(func) => {
                    // Builtin は「Vec<Value>」を受け取るので単引数なら vec![arg_val]
                    // func(vec![arg_val])
                    func(arg_val)
                }

                _ => panic!("attempted to apply non-function"),
            }
        }

        ExprBody::Block(items) => {
            let mut env2 = env.duplicate(); // 新しいスコープ
            let mut last_val = Value::Lit(Lit::Unit);
            for item in items {
                last_val = eval_item(&item, &mut env2);
            }
            last_val
        }

        ExprBody::If(e1, e2, e3) => {
            match eval_expr(&e1, env) {
                Value::Lit(Lit::Bool(true))  => eval_expr(&e2, env),
                Value::Lit(Lit::Bool(false)) => eval_expr(&e3, env),
                v => panic!("if condition must be Bool, got {}", v),
            }
        }

        ExprBody::Match(scrut, arms) => {
            let v_scrut = eval_expr(&scrut, env);
            for (pat, body) in arms {
                if let Some(bindings) = match_pat(&pat, &v_scrut) {
                    let env2 = env.duplicate();
                    env2.extend(&bindings);
                    return eval_expr(&body, &env2);
                }
            }
            panic!("non-exhaustive match: no pattern matched value {}", v_scrut);
        }

        ExprBody::Tuple(es) => {
            let xs: Vec<Value> = es.iter().map(|x| eval_expr(x, env)).collect();
            Value::Tuple(xs)
        }

        ExprBody::Record(fields) => {
            let mut vals = Vec::new();
            for (fname, fexpr) in fields {
                let v = eval_expr(&fexpr, env);
                vals.push((fname.clone(), v));
            }
            Value::Record(vals)
        }
        ExprBody::FieldAccess(base, field) => {
            let v_base = eval_expr(&base, env);
            match v_base {
                Value::Record(fields) => {
                    match fields.iter().find(|(name, _)| name == field) {
                        Some((_, val)) => val.clone(),
                        None => panic!("field '{}' not found in record", field),
                    }
                }
                Value::Con(_, args) if args.len() == 1 => {
                    match &args[0] {
                        Value::Record(fields) => {
                            match fields.iter().find(|(name, _)| name == field) {
                                Some((_, val)) => val.clone(),
                                None => panic!("field '{}' not found in record", field),
                            }
                        }
                        other => panic!("field access on non-record value: {}", other),
                    }
                }
                other => panic!("field access on non-record value: {}", other),
            }
        }
        ExprBody::TupleAccess(base, index) => {
            let v_base = eval_expr(&base, env);
            match v_base {
                Value::Tuple(elems) => {
                    if *index < elems.len() {
                        elems[*index].clone()
                    } else {
                        panic!("index out of bounds: {}", index)
                    }
                }
                Value::Con(_, args) if args.len() == 1 => {
                    match &args[0] {
                        Value::Tuple(elems) => {
                            if *index < elems.len() {
                                elems[*index].clone()
                            } else {
                                panic!("index out of bounds: {}", index)
                            }
                        }
                        other => panic!("index access on non-tuple value: {}", other),
                    }
                }
                other => panic!("index access on non-tuple value: {}", other),
            }
        }
        ExprBody::RawTraitRecord(_) => {
            unreachable!()
        }
    }
}

use crate::syntax::ast::Pat;
// use super::value::{Value, Env};

fn match_pat(pat: &Pat, val: &Value) -> Option<Binding> {
    let mut env = Binding::new();
    match (pat, val) {
        // ワイルドカード
        (Pat::Wildcard, _) => Some(env),

        // リテラルパターン
        (Pat::Lit(p), Value::Lit(v)) if p == v => Some(env),

        // 変数パターン
        (Pat::Var(name), v) => {
            env.insert(name.clone(), v.clone());
            Some(env)
        }

        // コンストラクタパターン
        (Pat::Con(name_p, args_p), Value::Con(name_v, args_v))
            if name_p == name_v && args_p.len() == args_v.len() =>
        {
            for (p, v) in args_p.iter().zip(args_v.iter()) {
                let sub = match_pat(p, v)?;
                env.extend(sub);
            }
            Some(env)
        }

        // タプルパターン
        (Pat::Tuple(pats), Value::Tuple(vals)) if pats.len() == vals.len() => {
            for (p, v) in pats.iter().zip(vals.iter()) {
                let sub = match_pat(p, v)?;
                env.extend(sub);
            }
            Some(env)
        }

        // レコードパターン（フィールド順不同）
        (Pat::Record(fields1), Value::Record(fields2)) => {
            for (fname, p) in fields1 {
                match fields2.iter().find(|(n, _)| n == fname) {
                    Some((_, v)) => {
                        let sub = match_pat(p, v)?;
                        env.extend(sub);
                    }
                    None => return None,
                }
            }
            Some(env)
        }

        // それ以外は失敗
        _ => None,
    }
}


use std::rc::Rc;
// use super::{Value, Env};
use crate::syntax::ast::Lit;

/// 評価時の初期環境
pub fn initial_env() -> Env {
    let env = Env::new();

    // 比較演算子
    env.insert("__i64_eq__".into(), make_i64_cmp_op(|a, b| a == b));
    env.insert("__i64_ne__".into(), make_i64_cmp_op(|a, b| a != b));
    env.insert("__i64_le__".into(), make_i64_cmp_op(|a, b| a <= b));
    env.insert("__i64_lt__".into(), make_i64_cmp_op(|a, b| a < b));
    env.insert("__i64_ge__".into(), make_i64_cmp_op(|a, b| a >= b));
    env.insert("__i64_gt__".into(), make_i64_cmp_op(|a, b| a > b));

    // 演算子
    env.insert("__i64_add__".into(), make_i64_arith_op(|a, b| a + b));
    env.insert("__i64_sub__".into(), make_i64_arith_op(|a, b| a - b));
    env.insert("__i64_mul__".into(), make_i64_arith_op(|a, b| a * b));
    env.insert("__i64_div__".into(), make_i64_arith_op(|a, b| {
        if b == 0 {
            panic!("division by zero");
        }
        a / b
    }));

    env.insert("__i64_neg__".into(), make_i64_unary_op(|x| -x));

    env
}

pub fn make_constructor(name: &str, arity: usize) -> Value {
    let name = name.to_string();
    // 部分適用を保持する内部関数
    fn curry(name: String, arity: usize, args: Vec<Value>) -> Value {
        if args.len() == arity {
            Value::Con(name, args)
        } else {
            Value::Builtin(Rc::new(move |arg: Value| {
                let mut new_args = args.clone();
                new_args.push(arg);
                curry(name.clone(), arity, new_args)
            }))
        }
    }
    curry(name, arity, vec![])
}

/// 単項の整数演算子をBuiltinとして作る
/// Int -> Int
pub fn make_i64_unary_op<F>(op: F) -> Value
where
    F: Fn(i64) -> i64 + 'static,
{
    Value::Builtin(Rc::new(move |arg: Value| {
        if let Value::Lit(Lit::Int(a)) = arg {
            return Value::Lit(Lit::Int(op(a)));
        }
        panic!("type error in <builtin>");
    }))
}

/// Int -> Int -> Int
pub fn make_i64_arith_op<F>(op: F) -> Value
where
    F: Fn(i64, i64) -> i64 + 'static,
{
    Value::Builtin(Rc::new(move |arg: Value| {
        if let Value::Tuple(xs) = arg {
            if let [Value::Lit(Lit::Int(a)), Value::Lit(Lit::Int(b))] = &xs[..] {
                return Value::Lit(Lit::Int(op(*a, *b)));
            }
        }
        panic!("type error in <builtin>");
    }))
}

/// 2引数の比較演算子をBuiltinとして作る
/// Int -> Int -> Bool
pub fn make_i64_cmp_op<F>(op: F) -> Value
where
    F: Fn(i64, i64) -> bool + 'static,
{
    Value::Builtin(Rc::new(move |arg: Value| {
        if let Value::Tuple(xs) = arg {
            if let [Value::Lit(Lit::Int(a)), Value::Lit(Lit::Int(b))] = &xs[..] {
                return Value::Lit(Lit::Bool(op(*a, *b)));
            }
        }
        panic!("type error in <builtin>");
    }))
}
