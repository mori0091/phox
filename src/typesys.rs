mod kind;
pub use kind::Kind;

mod apply_subst;
pub use apply_subst::ApplySubst;

mod free_type_vars;
pub use free_type_vars::FreeTypeVars;

mod repr;
pub use repr::Repr;

mod scheme_pretty;
pub use scheme_pretty::SchemePretty;
pub use scheme_pretty::Pretty;

mod type_;
pub use type_::{Type, TypeVarId};

mod trait_head;
pub use trait_head::TraitHead;

mod scheme;
pub use scheme::Scheme;
pub use scheme::generalize;

mod scheme_template;
pub use scheme_template::SchemeTemplate;

mod type_scheme;
pub use type_scheme::TypeScheme;

mod trait_scheme;
pub use trait_scheme::TraitScheme;

mod raw_type_scheme;
pub use raw_type_scheme::RawTypeScheme;

mod type_context;
pub use type_context::TypeContext;

mod infer_ctx;
pub use infer_ctx::{
    KindEnv,
    TypeEnv,
    TraitMemberEnv,
    InferCtx,
};

mod infer;
pub use infer::{
    infer_item,
    infer_stmt,
    infer_expr,
};

mod impl_env;
pub use impl_env::ImplEnv;

mod apply_trait_impls;
pub use apply_trait_impls::{
    apply_trait_impls_item,
    apply_trait_impls_stmt,
    apply_trait_impls_expr,
};

mod error;
pub use error::TypeError;
