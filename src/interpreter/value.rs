use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use crate::syntax::ast::{Expr, Lit};

/// 評価時の環境
#[derive(Clone)]
pub struct Env {
    map: Rc<RefCell<HashMap<String, Value>>>,
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

    pub fn extend(&self, other: &Env) {
        self.map.borrow_mut().extend(other.map.borrow().clone());
    }

    pub fn clone_map(&self) -> HashMap<String, Value> {
        self.map.borrow().clone()
    }

    pub fn duplicate(&self) -> Env {
        Env { map: Rc::new(RefCell::new(self.clone_map())) }
    }
}

#[derive(Clone)]
pub enum Value {
    Lit(Lit),
    Closure { param: String, body: Box<Expr>, env: Env },
    Con(String, Vec<Value>),
    Builtin(Rc<dyn Fn(Vec<Value>) -> Value>), // ← Rust 側の関数をラップ
    Tuple(Vec<Value>),
    Struct(String, Vec<(String, Value)>),
}

use std::fmt;

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Lit(lit) => write!(f, "{lit}"),

            Value::Tuple(vals) => {
                match vals.len() {
                    0 => unreachable!("empty tuble"), // 通常は Lit::Unit で処理されるはず
                    1 => write!(f, "({},)", vals[0]),
                    _ => {
                        let inner: Vec<String> = vals.iter().map(|v| v.to_string()).collect();
                        write!(f, "({})", inner.join(", "))
                    }
                }
            }

            Value::Struct(_, _) => todo!(),

            Value::Con(name, args) => {
                if args.is_empty() {
                    write!(f, "{name}")
                } else {
                    // 引数が複雑なら括弧を付ける
                    let inner: Vec<String> = args.iter().map(|v| {
                        match v {
                            Value::Lit(_)  => v.to_string(),
                            Value::Con(_, ref a) if a.is_empty() => v.to_string(),
                            _ => format!("({})", v),
                        }
                    }).collect();
                    write!(f, "{} {}", name, inner.join(" "))
                }
            }

            Value::Closure { .. } => write!(f, "<closure>"),
            Value::Builtin(_) => write!(f, "<builtin>"),
        }
    }
}
