use crate::syntax::ast::Expr;
use super::{Value, Env, Binding};

/// 評価関数
pub fn eval(expr: &Expr, env: &Env) -> Value {
    match expr {
        // リテラル
        Expr::Lit(lit) => Value::Lit(lit.clone()),

        // 変数参照
        Expr::Var(name) => env.get(name)
            .unwrap_or_else(|| panic!("unbound variable: {}", name)),

        // λ抽象
        Expr::Abs(param, body) => {
            Value::Closure {
                param: param.clone(),
                body: body.clone(),
                env: env.clone(), // クロージャは定義時の環境をキャプチャ（Rc<RefCell<_>>共有）
            }
        }

        // 関数適用
        Expr::App(f, arg) => {
            let f_val = eval(f, env);
            let arg_val = eval(arg, env);
            match f_val {
                // ユーザ定義関数（Closure）
                Value::Closure { param, body, env: closure_env } => {
                    let env2 = closure_env.duplicate();
                    env2.insert(param, arg_val);
                    eval(&body, &env2)
                }

                // 組み込み関数（Builtin）
                Value::Builtin(func) => {
                    // Builtin は「Vec<Value>」を受け取るので単引数なら vec![arg_val]
                    func(vec![arg_val])
                }

                // _ => panic!("attempted to apply non-function value: {:?}", f_val),
                _ => panic!("attempted to apply non-function value: {}", f),
            }
        }

        Expr::Let(pat, e1, e2) => {
            let v1 = eval(e1, env);
            if let Some(new_bindings) = match_pat(pat, &v1) {
                let env2 = env.duplicate();
                env2.extend(&new_bindings);
                eval(e2, &env2)
            } else {
                panic!("pattern match failed in let");
            }
        }

        Expr::LetRec(pat, e1, e2) => {
            match pat {
                Pat::Var(x) => {
                    let env2 = env.clone();

                    // 仮の未初期化プレースホルダ（呼ばれたら明示的にエラー）
                    env2.insert(x.clone(), Value::Builtin(Rc::new(|_| {
                        // panic!("recursive value '{}' used before initialization", x)
                        panic!("recursive value used before initialization")
                    })));

                    // 本体を評価（env2 で自分を参照できる）
                    let v1 = eval(e1, &env2);

                    // 完成した値に置き換え
                    env2.insert(x.clone(), v1);

                    // 続きの式を評価
                    eval(e2, &env2)
                }
                _ => panic!("let rec pattern not supported (only variable)"),
            }
        }

        Expr::If(e1, e2, e3) => {
            match eval(e1, env) {
                Value::Lit(Lit::Bool(true))  => eval(e2, env),
                Value::Lit(Lit::Bool(false)) => eval(e3, env),
                v => panic!("if condition must be Bool, got {}", v),
            }
        }

        Expr::Match(scrut, arms) => {
            let v_scrut = eval(scrut, env);
            for (pat, body) in arms {
                if let Some(bindings) = match_pat(pat, &v_scrut) {
                    let env2 = env.duplicate();
                    env2.extend(&bindings);
                    return eval(body, &env2);
                }
            }
            panic!("non-exhaustive match: no pattern matched value {}", v_scrut);
        }

        Expr::Tuple(es) => {
            let xs: Vec<Value> = es.iter().map(|x| eval(x, env)).collect();
            Value::Tuple(xs)
        }

        Expr::Struct(name, fields) => {
            let mut vals = Vec::new();
            for (fname, fexpr) in fields {
                let v = eval(fexpr, env);
                vals.push((fname.clone(), v));
            }
            Value::Struct(name.clone(), vals)
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

        // 構造体パターン（フィールド順不同）
        (Pat::Struct(name1, fields1), Value::Struct(name2, fields2))
            if name1 == name2 =>
        {
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

    // Option
    env.insert("None".into(), Value::Con("None".into(), vec![]));
    env.insert("Some".into(), make_constructor("Some", 1));

    // List
    env.insert("Nil".into(), Value::Con("Nil".into(), vec![]));
    env.insert("Cons".into(), make_constructor("Cons", 2));

    // 演算子
    env.insert("+".into(), make_binop(|a, b| a + b));
    env.insert("-".into(), make_binop(|a, b| a - b));
    env.insert("*".into(), make_binop(|a, b| a * b));
    env.insert("/".into(), make_binop(|a, b| {
        if b == 0 {
            panic!("division by zero");
        }
        a / b
    }));

    // 比較演算子
    env.insert("==".into(), make_cmpop(|a, b| a == b));
    env.insert("!=".into(), make_cmpop(|a, b| a != b));
    env.insert("<".into() , make_cmpop(|a, b| a < b));
    env.insert("<=".into(), make_cmpop(|a, b| a <= b));
    env.insert(">".into() , make_cmpop(|a, b| a > b));
    env.insert(">=".into(), make_cmpop(|a, b| a >= b));

    env
}

pub fn make_constructor(name: &str, arity: usize) -> Value {
    let name = name.to_string();
    // 部分適用を保持する内部関数
    fn curry(name: String, arity: usize, args: Vec<Value>) -> Value {
        if args.len() == arity {
            Value::Con(name, args)
        } else {
            Value::Builtin(Rc::new(move |mut more: Vec<Value>| {
                let mut new_args = args.clone();
                new_args.append(&mut more);
                curry(name.clone(), arity, new_args)
            }))
        }
    }
    curry(name, arity, vec![])
}

/// 2引数の整数演算子をBuiltinとして作る
/// Int -> Int -> Int
pub fn make_binop<F>(op: F) -> Value
where
    F: Fn(i64, i64) -> i64 + 'static,
{
    fn curry<F>(op: Rc<F>, args: Vec<Value>) -> Value
    where
        F: Fn(i64, i64) -> i64 + 'static,
    {
        if args.len() == 2 {
            match (&args[0], &args[1]) {
                (Value::Lit(Lit::Int(a)), Value::Lit(Lit::Int(b))) => {
                    Value::Lit(Lit::Int(op(*a, *b)))
                }
                _ => panic!("type error: expected Int arguments"),
            }
        } else {
            Value::Builtin(Rc::new(move |mut more: Vec<Value>| {
                let mut new_args = args.clone();
                new_args.append(&mut more);
                curry(op.clone(), new_args)
            }))
        }
    }

    let op = Rc::new(op);
    curry(op, vec![])
}

/// 2引数の比較演算子をBuiltinとして作る
/// Int -> Int -> Bool
pub fn make_cmpop<F>(op: F) -> Value
where
    F: Fn(i64, i64) -> bool + 'static,
{
    fn curry<F>(op: Rc<F>, args: Vec<Value>) -> Value
    where
        F: Fn(i64, i64) -> bool + 'static,
    {
        if args.len() == 2 {
            match (&args[0], &args[1]) {
                (Value::Lit(Lit::Int(a)), Value::Lit(Lit::Int(b))) => {
                    Value::Lit(Lit::Bool(op(*a, *b)))
                }
                _ => panic!("type error: expected Int arguments"),
            }
        } else {
            Value::Builtin(Rc::new(move |mut more: Vec<Value>| {
                let mut new_args = args.clone();
                new_args.append(&mut more);
                curry(op.clone(), new_args)
            }))
        }
    }

    let op = Rc::new(op);
    curry(op, vec![])
}
