use crate::error::Error;

// -------------------------------------------------------------
mod make_resolved_symbol;
pub use make_resolved_symbol::*;

// -------------------------------------------------------------
mod resolve_symbol;
pub use resolve_symbol::*;

// -------------------------------------------------------------
use indexmap::IndexMap;
pub type TyParMap = IndexMap<String, TypeVarId>;

mod resolve_item;
pub use resolve_item::*;

// -------------------------------------------------------------
mod resolve_decl;
pub use resolve_decl::*;

mod resolve_stmt;
pub use resolve_stmt::*;

mod resolve_expr;
pub use resolve_expr::*;

mod resolve_pat;
pub use resolve_pat::*;

// -------------------------------------------------------------
use crate::api::PhoxEngine;
use crate::module::*;
use crate::syntax::ast::*;
use crate::typesys::*;

mod decl_type_def;
use decl_type_def::*;

mod decl_trait;
use decl_trait::*;

mod decl_impl;
use decl_impl::*;

mod decl_starlet;
use decl_starlet::*;

mod stmt_use;
use stmt_use::*;
