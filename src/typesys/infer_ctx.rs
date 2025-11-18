use std::collections::{HashMap, HashSet};
use super::*;
use crate::module::*;

// ===== Kind Environment =====
// maps name of type constructor to Kind
pub type KindEnv = HashMap<Symbol, Kind>;

// ===== Type Environment =====
// maps name of variable to type scheme.
// ex.
// {
//   "id": ∀ a. a -> a,
// }
pub type TypeEnv = HashMap<Symbol, TypeScheme>;

// ===== Type Environment for trait member =====
// maps name of trait member to set of type schemes.
//
// ex.
//
// trait_member_env: TraitMemberEnv = {
//   "bind": {
//     ∀ m. Monad m => (∀ a b. m a -> (a -> m b) -> m b),
//   },
// }
//
// ===== Type Environment for impl member =====
// maps name of impl member to set of **raw** type schemes.
//
// ex.
//
// impl_member_env: TraitMemberEnv = {
//   "bind": {
//          Monad Option      => (∀ a b. Option a -> (a -> Option b) -> Option b),
//          Monad (Result ()) => (∀ a b. Result () a -> (a -> Result () b) -> Result () b),
//     ∀ e. Monad (Result e)  => (∀ a b. Result e a -> (a -> Result e b) -> Result e b),
//   },
// }
//
pub type TraitMemberEnv = HashMap<Symbol, HashSet<SchemeTemplate<Type>>>;

// ===== Infer Context =====
#[derive(Clone)]
pub struct InferCtx {
    pub kind_env: KindEnv,            // 型コンストラクタの kind 情報 (ex. List: * -> *)
    pub type_env: TypeEnv,            // スコープ内の識別子の型スキーム (ex. `==`: ∀ a. a -> a -> Bool)
    pub trait_member_env: TraitMemberEnv, // traitメンバの型スキーム集合 (ex. "f": { ∀ a. Foo a => a -> a, ∀ a. Bar a => a -> a })
}

impl InferCtx {
    pub fn new() -> Self {
        Self {
            kind_env: KindEnv::new(),
            type_env: TypeEnv::new(),
            trait_member_env: TraitMemberEnv::new(),
        }
    }

    pub fn free_env_vars(&self, ctx: &mut TypeContext) -> HashSet<TypeVarId> {
        let mut acc = HashSet::new();
        for scheme in self.type_env.values() {
            scheme.target.free_type_vars(ctx, &mut acc);
            for v in &scheme.vars {
                acc.remove(v);
            }
        }
        acc
    }
}
