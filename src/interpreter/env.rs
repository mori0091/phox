use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use super::Value;
use crate::syntax::ast::Lit;

pub type Binding = HashMap<String, Value>;

/// 評価時の環境
#[derive(Clone)]
pub struct Env {
    map: Rc<RefCell<Binding>>,
}

impl Env {
    pub fn new() -> Self {
        Env { map: Rc::new(RefCell::new(HashMap::new())) }
    }

    pub fn insert(&self, k: String, v: Value) {
        self.map.borrow_mut().insert(k, v);
    }

    pub fn get(&self, k: &str) -> Option<Value> {
        self.map.borrow().get(k).cloned()
    }

    pub fn extend(&self, other: &Binding) {
        self.map.borrow_mut().extend(other.clone());
    }

    pub fn clone_map(&self) -> Binding {
        self.map.borrow().clone()
    }

    pub fn duplicate(&self) -> Env {
        Env { map: Rc::new(RefCell::new(self.clone_map())) }
    }
}

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
