use std::rc::Rc;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use crate::module::*;
use super::*;

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
pub struct InnerInferCtx {
    pub kind_env: KindEnv,            // 型コンストラクタの kind 情報 (ex. List: * -> *)
    pub type_env: TypeEnv,            // スコープ内の識別子の型スキーム (ex. `==`: ∀ a. a -> a -> Bool)
    pub trait_member_env: TraitMemberEnv, // traitメンバの型スキーム集合 (ex. "f": { ∀ a. Foo a => a -> a, ∀ a. Bar a => a -> a })
}

impl InnerInferCtx {
    fn new() -> Self {
        Self {
            kind_env: KindEnv::new(),
            type_env: TypeEnv::new(),
            trait_member_env: TraitMemberEnv::new(),
        }
    }
}

#[derive(Clone)]
pub struct InferCtx {
    pub inner: Rc<RefCell<InnerInferCtx>>,
    local: bool,
    in_template_body: bool,
}

impl InferCtx {
    pub fn new() -> Self {
        Self {
            inner: Rc::new(RefCell::new(InnerInferCtx::new())),
            local: false,
            in_template_body: false,
        }
    }

    pub fn duplicate(&self) -> InferCtx {
        InferCtx {
            inner: Rc::new(RefCell::new(self.clone_inner())),
            local: true,
            in_template_body: self.in_template_body,
        }
    }

    pub fn clone_inner(&self) -> InnerInferCtx {
        self.inner.borrow().clone()
    }

    pub fn is_local(&self) -> bool {
        self.local
    }

    pub fn is_in_template_body(&self) -> bool {
        self.in_template_body
    }
    pub fn enter_template_body(&mut self) {
        self.in_template_body = true;
    }
    pub fn leave_template_body(&mut self) {
        self.in_template_body = false;
    }
}

impl InferCtx {
    pub fn put_kind(&self, symbol: Symbol, kind: Kind) {
        self.inner.borrow_mut().kind_env.insert(symbol, kind);
    }
    pub fn get_kind(&self, symbol: &Symbol) -> Option<Kind> {
        self.inner.borrow().kind_env.get(symbol).cloned()
    }

    pub fn put_type_scheme(&self, symbol: Symbol, sch: TypeScheme) {
        self.inner.borrow_mut().type_env.insert(symbol, sch);
    }
    pub fn get_type_scheme(&self, symbol: &Symbol) -> Option<TypeScheme> {
        self.inner.borrow().type_env.get(symbol).cloned()
    }

    pub fn put_trait_member_scheme(&self, symbol: Symbol, tmpl: SchemeTemplate<Type>) {
        self.inner
            .borrow_mut()
            .trait_member_env
            .entry(symbol)
            .or_default()
            .insert(tmpl);
    }
    pub fn extend_trait_member_schemes(&self, symbol: &Symbol, tmpls: HashSet<SchemeTemplate<Type>>) {
        self.inner
            .borrow_mut()
            .trait_member_env
            .entry(symbol.clone())
            .or_default()
            .extend(tmpls);
    }
    pub fn get_trait_member_schemes(&self, symbol: &Symbol) -> Option<HashSet<SchemeTemplate<Type>>> {
        self.inner
            .borrow()
            .trait_member_env
            .get(symbol)
            .cloned()
    }
    pub fn is_trait_member(&self, symbol: &Symbol) -> bool {
        self.inner
            .borrow()
            .trait_member_env
            .contains_key(symbol)
    }

    pub fn free_env_vars(&self, ctx: &mut TypeContext) -> HashSet<TypeVarId> {
        let mut acc = HashSet::new();
        for scheme in self.inner.borrow().type_env.values() {
            scheme.target.free_type_vars(ctx, &mut acc);
            for v in &scheme.vars {
                acc.remove(v);
            }
        }
        acc
    }
}
