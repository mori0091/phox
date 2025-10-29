mod kind;
pub use kind::Kind;

mod apply_subst;
pub use apply_subst::ApplySubst;

mod free_type_vars;
pub use free_type_vars::FreeTypeVars;

mod repr;
pub use repr::Repr;

mod type_;
pub use type_::{Type, TypeVarId};

mod constraint;
pub use constraint::Constraint;

mod scheme;
pub use scheme::Scheme;

mod type_scheme;
pub use type_scheme::TypeScheme;

mod trait_scheme;
pub use trait_scheme::TraitScheme;

mod infer;
pub use infer::{TypeContext, KindEnv, TypeEnv, TraitMemberEnv, InferCtx, ImplEnv};
pub use infer::{initial_kind_env, initial_type_env};
pub use infer::{infer_item, infer_stmt, infer_expr};
pub use infer::{instantiate, generalize};

mod apply_trait_impls;
pub use apply_trait_impls::{
    apply_trait_impls_item,
    apply_trait_impls_stmt,
    apply_trait_impls_expr,
};

mod error;
pub use error::TypeError;
