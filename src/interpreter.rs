use crate::error::Error;

mod env;
pub use env::ValueEnv;

mod value;
pub use value::Value;

mod builtin;
pub use builtin::*;

pub mod eval;
pub use eval::{
    eval_item,
    eval_stmt,
    eval_expr
};
