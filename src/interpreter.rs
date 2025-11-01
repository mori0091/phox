mod env;
pub use env::{Env, Binding};

mod value;
pub use value::Value;

pub mod eval;
pub use eval::{eval_item, eval_stmt, eval_expr, initial_env};
pub use eval::{make_i64_arith_op, make_i64_cmp_op, make_constructor};
