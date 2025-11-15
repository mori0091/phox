use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use super::Value;
use crate::syntax::ast::Lit;
use crate::module::*;

pub type Binding = HashMap<Symbol, Value>;

/// 評価時の環境
#[derive(Clone)]
pub struct ValueEnv {
    map: Rc<RefCell<Binding>>,
}

impl ValueEnv {
    pub fn new() -> Self {
        ValueEnv { map: Rc::new(RefCell::new(HashMap::new())) }
    }

    pub fn insert(&self, k: Symbol, v: Value) {
        self.map.borrow_mut().insert(k, v);
    }

    pub fn get(&self, k: &Symbol) -> Option<Value> {
        self.map.borrow().get(k).cloned()
    }

    pub fn extend(&self, other: &Binding) {
        self.map.borrow_mut().extend(other.clone());
    }

    pub fn clone_map(&self) -> Binding {
        self.map.borrow().clone()
    }

    pub fn duplicate(&self) -> ValueEnv {
        ValueEnv { map: Rc::new(RefCell::new(self.clone_map())) }
    }
}

/// 評価時の初期環境
pub fn initial_env() -> ValueEnv {
    let env = ValueEnv::new();

    // 比較演算子
    env.insert(Symbol::Local("__i64_eq__".into()), make_i64_cmp_op(|a, b| a == b));
    env.insert(Symbol::Local("__i64_ne__".into()), make_i64_cmp_op(|a, b| a != b));
    env.insert(Symbol::Local("__i64_le__".into()), make_i64_cmp_op(|a, b| a <= b));
    env.insert(Symbol::Local("__i64_lt__".into()), make_i64_cmp_op(|a, b| a < b));
    env.insert(Symbol::Local("__i64_ge__".into()), make_i64_cmp_op(|a, b| a >= b));
    env.insert(Symbol::Local("__i64_gt__".into()), make_i64_cmp_op(|a, b| a > b));

    // 演算子
    env.insert(Symbol::Local("__i64_add__".into()), make_i64_arith_op(|a, b| a + b));
    env.insert(Symbol::Local("__i64_sub__".into()), make_i64_arith_op(|a, b| a - b));
    env.insert(Symbol::Local("__i64_mul__".into()), make_i64_arith_op(|a, b| a * b));
    env.insert(Symbol::Local("__i64_div__".into()), make_i64_arith_op(|a, b| {
        if b == 0 {
            panic!("division by zero");
        }
        a / b
    }));

    env.insert(Symbol::Local("__i64_neg__".into()), make_i64_unary_op(|x| -x));

    env
}

pub fn make_constructor(name: &Symbol, arity: usize) -> Value {
    // 部分適用を保持する内部関数
    fn curry(name: Symbol, arity: usize, args: Vec<Value>) -> Value {
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
    curry(name.clone(), arity, vec![])
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
