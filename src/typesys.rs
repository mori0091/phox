use crate::error::Error;

// -------------------------------------------------------------
mod apply_subst;
pub use apply_subst::*;

mod free_vars;
pub use free_vars::*;

mod repr;
pub use repr::*;

mod rename_for_pretty;
pub use rename_for_pretty::*;

mod pretty;
pub use pretty::*;

// -------------------------------------------------------------
mod kind;
pub use kind::*;

mod type_;
pub use type_::*;

mod type_var_id;
pub use type_var_id::*;

mod type_expr;
pub use type_expr::*;

mod trait_head;
pub use trait_head::*;

// -------------------------------------------------------------
mod scheme;
pub use scheme::*;

mod scheme_template;
pub use scheme_template::*;

mod type_scheme;
pub use type_scheme::*;

mod trait_scheme;
pub use trait_scheme::*;

// -------------------------------------------------------------
mod type_constraint;
pub use type_constraint::*;

mod constraint;
pub use constraint::*;

mod constraint_set;
pub use constraint_set::*;

// -------------------------------------------------------------
mod solver;
pub use solver::*;

mod solve_item;
pub use solve_item::*;

// -------------------------------------------------------------
mod context;
pub use context::*;

mod type_context;
pub use type_context::*;

// ----
mod unified_context;
pub use unified_context::*;

// ----
mod infer_ctx;
pub use infer_ctx::{
    KindEnv,
    TypeEnv,
    TraitMemberEnv,
    InferCtx,
};

// ----
mod impl_env;
pub use impl_env::*;

// ----
mod starlet_env;
pub use starlet_env::*;

// -------------------------------------------------------------
mod infer;
pub use infer::{
    infer_item,
    infer_stmt,
    infer_expr,
};

mod register;
pub use register::*;

mod apply_trait_impls;
pub use apply_trait_impls::*;
