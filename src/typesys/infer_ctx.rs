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
pub type TraitMemberEnv = HashMap<Symbol, HashSet<RawTypeScheme>>;

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

    pub fn initial() -> Self {
        Self {
            kind_env: initial_kind_env(),
            type_env: initial_type_env(),
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

// -----
pub fn initial_kind_env() -> KindEnv {
    let mut env = KindEnv::new();

    env.insert(Symbol::Local("()".into()), Kind::Star);
    env.insert(Symbol::Local("Int".into()), Kind::Star);
    env.insert(Symbol::Local("Bool".into()), Kind::Star);

    env
}

pub fn initial_type_env() -> TypeEnv {
    let mut type_env = TypeEnv::new();

    type_env.insert(
        Symbol::Local("__i64_eq__".into()),
        TypeScheme {
            vars: vec![],
            constraints: vec![],
            target: Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::bool_()), // (Int, Int) -> Bool
        },
    );

    type_env.insert(
        Symbol::Local("__i64_ne__".into()),
        TypeScheme {
            vars: vec![],
            constraints: vec![],
            target: Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::bool_()), // (Int, Int) -> Bool
        },
    );

    type_env.insert(
        Symbol::Local("__i64_le__".into()),
        TypeScheme {
            vars: vec![],
            constraints: vec![],
            target: Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::bool_()), // (Int, Int) -> Bool
        },
    );

    type_env.insert(
        Symbol::Local("__i64_lt__".into()),
        TypeScheme {
            vars: vec![],
            constraints: vec![],
            target: Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::bool_()), // (Int, Int) -> Bool
        },
    );

    type_env.insert(
        Symbol::Local("__i64_ge__".into()),
        TypeScheme {
            vars: vec![],
            constraints: vec![],
            target: Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::bool_()), // (Int, Int) -> Bool
        },
    );

    type_env.insert(
        Symbol::Local("__i64_gt__".into()),
        TypeScheme {
            vars: vec![],
            constraints: vec![],
            target: Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::bool_()), // (Int, Int) -> Bool
        },
    );

    type_env.insert(
        Symbol::Local("__i64_add__".into()),
        TypeScheme {
            vars: vec![],
            constraints: vec![],
            target: Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::int()), // (Int, Int) -> Int
        },
    );

    type_env.insert(
        Symbol::Local("__i64_sub__".into()),
        TypeScheme {
            vars: vec![],
            constraints: vec![],
            target: Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::int()), // (Int, Int) -> Int
        },
    );

    type_env.insert(
        Symbol::Local("__i64_mul__".into()),
        TypeScheme {
            vars: vec![],
            constraints: vec![],
            target: Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::int()), // (Int, Int) -> Int
        },
    );

    type_env.insert(
        Symbol::Local("__i64_div__".into()),
        TypeScheme {
            vars: vec![],
            constraints: vec![],
            target: Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::int()), // (Int, Int) -> Int
        },
    );

    type_env.insert(
        Symbol::Local("__i64_neg__".into()),
        TypeScheme {
            vars: vec![],
            constraints: vec![],
            target: Type::fun(Type::int(), Type::int()),
        },
    );

    type_env
}
