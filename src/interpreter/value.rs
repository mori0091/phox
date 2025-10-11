use std::rc::Rc;
use crate::syntax::ast::{Expr, Lit};
use super::Env;

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

            Value::Struct(name, fields) => {
                if fields.is_empty() {
                    write!(f, "struct {} {{}}", name)
                }
                else {
                    let s: Vec<String>
                        = fields.iter()
                                .map(|(k, v)| format!("{}: {}", k, v))
                                .collect();
                    write!(f, "struct {} {{ {} }}", name, s.join(", "))
                }
            }

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
