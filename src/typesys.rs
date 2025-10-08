mod kind;
pub use kind::Kind;

mod type_;
pub use type_::{Type, TypeVarId};

mod scheme;
pub use scheme::Scheme;

mod infer;
pub use infer::{TypeContext, TypeError};
pub use infer::{initial_kind_env, initial_type_env};
pub use infer::{infer, generalize};
