use std::collections::HashMap;
use crate::syntax::ast::*;
use crate::module::*;
use super::*;

// ===== Impl Environment =====
// maps instance of trait to dictionary of its implementations.
// ex.
// impl_env: ImplEnv = {
//   (Monad Option): {
//     "pure": \a. Some a,
//     "bind": \a.\f. match (a) {
//       None => None,
//       Some x => f x,
//     },
//   },
//
//   (Monad (Result ())): {
//     "pure": \a. Ok a,
//     "bind": \a.\f. match (a) {
//       Err () => Err (),
//       Ok ok  => f ok,
//     },
//   },
//
//   (∀ e. Monad (Result e)): {
//     "pure": \a. Ok a,
//     "bind": \a.\f. match (a) {
//       Err err => Err err,
//       Ok ok  => f ok,
//     },
//   },
// }
pub type ImplEnv = HashMap<TraitScheme, HashMap<Symbol, Expr>>;
