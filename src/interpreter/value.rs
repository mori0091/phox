use std::rc::Rc;
use crate::syntax::ast::*;
use crate::module::*;
use super::*;

#[derive(Clone)]
pub enum Value {
    Lit(Lit),
    Closure { pat: Pat, body: Box<Expr>, env: ValueEnv },
    Con(Symbol, Vec<Value>),
    Builtin(Rc<dyn Fn(Value) -> Value>), // ← Rust 側の関数をラップ
    Loop { pred: Box<Value>, next: Box<Value> },

    Tuple(Vec<Value>),
    Record(Vec<(String, Value)>),
}

use std::fmt;

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Lit(l) => write!(f, "{:?}", l),
            Value::Closure { .. } => write!(f, "<closure>"),
            Value::Con(name, _) => write!(f, "<con {}>", name),
            Value::Builtin(_) => write!(f, "<builtin>"),
            Value::Loop { pred:_, next:_ } => write!(f, "<loop>"),

            Value::Tuple(vs) => f.debug_tuple("Tuple").field(vs).finish(),
            Value::Record(fields) => f.debug_map()
                .entries(fields.iter().map(|(n, v)| (n, v)))
                .finish(),
        }
    }
}

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

            Value::Record(fields) => {
                if fields.is_empty() {
                    write!(f, "@{{}}")
                }
                else {
                    let s: Vec<String>
                        = fields.iter()
                                .map(|(k, v)| format!("{} = {}", PathComponent::Name(k.clone()).pretty(), v))
                                .collect();
                    write!(f, "@{{ {} }}", s.join(", "))
                }
            }

            Value::Con(name, args) => {
                if args.is_empty() {
                    write!(f, "{name}")
                }
                else {
                    // 引数が複雑なら括弧を付ける
                    let inner: Vec<String> = args.iter().map(|v| {
                        match v {
                            Value::Lit(_) | Value::Tuple(_) | Value::Record(_)  => v.to_string(),
                            Value::Con(_, ref a) if a.is_empty() => v.to_string(),
                            _ => format!("({})", v),
                        }
                    }).collect();
                    write!(f, "{} {}", name, inner.join(" "))
                }
            }

            Value::Closure { .. } => write!(f, "<closure>"),
            Value::Builtin(_) => write!(f, "<builtin>"),
            Value::Loop { pred:_, next:_ } => write!(f, "<loop>"),
        }
    }
}
