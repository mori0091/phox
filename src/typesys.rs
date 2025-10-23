mod kind;
pub use kind::Kind;

mod type_;
pub use type_::{Type, TypeVarId};

mod scheme;
pub use scheme::{Constraint, Scheme};

mod infer;
pub use infer::{KindEnv, TypeEnv, TraitMemberEnv, InferCtx, ImplEnv};
pub use infer::{TypeContext, TypeError};
pub use infer::{initial_kind_env, initial_type_env};
pub use infer::{infer_item, infer_stmt, infer_expr};
pub use infer::{instantiate, generalize};

mod apply_trait_impls;
pub use apply_trait_impls::{
    apply_trait_impls_item,
    apply_trait_impls_stmt,
    apply_trait_impls_expr,
};
