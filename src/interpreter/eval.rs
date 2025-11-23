use std::rc::Rc;

use crate::api::PhoxEngine;
use crate::module::*;
use crate::typesys::*;
use crate::syntax::ast::*;
use super::*;

pub fn eval_item(
    phox: &mut PhoxEngine,
    module: &RefModule,
    env: &mut ValueEnv,
    item: &Item,
) -> Value {
    match item {
        Item::Decl(_) => {
            // ignore
            Value::Lit(Lit::Unit)
        }
        Item::Stmt(stmt) => {
            eval_stmt(phox, module, env, stmt);
            Value::Lit(Lit::Unit)
        }
        Item::Expr(expr) => {
            eval_expr(phox, module, env, expr)
        }
    }
}

pub fn eval_stmt(
    phox: &mut PhoxEngine,
    module: &RefModule,
    env: &mut ValueEnv,
    stmt: &Stmt,
) {
    match stmt {
        Stmt::Use(_) => {},
        Stmt::Mod(name, items) => {
            let sub = &module.get_submod(name).unwrap();
            if let Some(items) = items {
                let env2 = &mut phox.get_value_env(sub);
                for mut item in items {
                    eval_item(phox, sub, env2, &mut item);
                }
                // let mut last = None;
                // for mut item in items {
                //     let res = eval_item(phox, &sub, env2, &mut item);
                //     last = Some(res);
                // }
                // last.ok_or_else(|| "program contained no expression".to_string())
            }
        },
        Stmt::Let(pat, expr) => {
            let val = eval_expr(phox, module, env, expr);
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
                    let val = eval_expr(phox, module, env, expr);
                    env.insert(x.clone(), val);
                }
                _ => panic!("let rec pattern not supported (only variable)"),
            }
        }
    }
}

/// 評価関数
pub fn eval_expr(
    phox: &mut PhoxEngine,
    module: &RefModule,
    env: &mut ValueEnv,
    expr: &Expr,
) -> Value {
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
                       panic!("unbound variable: {}: {}", name.pretty(), sch.pretty())
                   } else {
                       panic!("unbound variable: {}", name.pretty())
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
            let f_val = eval_expr(phox, module, env, &f);
            let arg_val = eval_expr(phox, module, env, &arg);
            match f_val {
                // ユーザ定義関数（Closure）
                Value::Closure { pat, body, env: closure_env } => {
                    if let Some(bindings) = match_pat(&pat, &arg_val) {
                        let env2 = &mut closure_env.duplicate();
                        env2.extend(&bindings);
                        eval_expr(phox, module, env2, &body)
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
            let env2 = &mut env.duplicate(); // 新しいスコープ
            let mut last_val = Value::Lit(Lit::Unit);
            for item in items {
                last_val = eval_item(phox, module, env2, &item);
            }
            last_val
        }

        ExprBody::If(e1, e2, e3) => {
            match eval_expr(phox, module, env, &e1) {
                Value::Lit(Lit::Bool(true))  => eval_expr(phox, module, env, &e2),
                Value::Lit(Lit::Bool(false)) => eval_expr(phox, module, env, &e3),
                v => panic!("if condition must be Bool, got {}", v),
            }
        }

        ExprBody::Match(scrut, arms) => {
            let v_scrut = eval_expr(phox, module, env, &scrut);
            for (pat, body) in arms {
                if let Some(bindings) = match_pat(&pat, &v_scrut) {
                    let env2 = &mut env.duplicate();
                    env2.extend(&bindings);
                    return eval_expr(phox, module, env2, &body);
                }
            }
            panic!("non-exhaustive match: no pattern matched value {}", v_scrut);
        }

        ExprBody::Tuple(es) => {
            let xs: Vec<Value> = es
                .iter()
                .map(|x| eval_expr(phox, module, env, x))
                .collect();
            Value::Tuple(xs)
        }

        ExprBody::Record(fields) => {
            let mut vals = Vec::new();
            for (fname, fexpr) in fields {
                let v = eval_expr(phox, module, env, &fexpr);
                vals.push((fname.clone(), v));
            }
            Value::Record(vals)
        }
        ExprBody::FieldAccess(base, field) => {
            let v_base = eval_expr(phox, module, env, &base);
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
            let v_base = eval_expr(phox, module, env, &base);
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

fn match_pat(pat: &Pat, val: &Value) -> Option<env::Binding> {
    let mut env = env::Binding::new();
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
