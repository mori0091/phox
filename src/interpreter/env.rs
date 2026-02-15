use std::rc::Rc;
use indexmap::IndexMap;

use super::Value;
use crate::module::*;
use crate::collection::RefMap;

pub type Binding = IndexMap<Symbol, Value>;
pub type ValueEnv = RefMap<Symbol, Value>;

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
