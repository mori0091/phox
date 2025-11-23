use super::*;

// -------------------------------------------------------------
#[derive(Clone, Debug)]
pub enum TypeDef {
    SumType {
        name: Symbol,           // Type constructor name of the ADT type
        params: Vec<TypeVarId>, // Type variables of the ADT type
        variants: Vec<Variant>, // Constructor variants
    }
}

// -------------------------------------------------------------
#[derive(Clone, Debug)]
pub enum Variant {
    Unit(Symbol),               // ex. `None, `Nil`,
    Tuple(Symbol, Vec<Type>),   // ex. `Some a`, `Result a e`,
}

impl Variant {
    pub fn name(&self) -> Symbol {
        match self {
            Variant::Unit(n) | Variant::Tuple(n, _) => n.clone(),
        }
    }
    pub fn arity(&self) -> usize {
        match self {
            Variant::Unit(_) => 0,
            Variant::Tuple(_, ts) => ts.len(),
        }
    }

    pub fn as_scheme(&self, type_name: &Symbol, params: &[TypeVarId]) -> TypeScheme {
        // 型コンストラクタ適用: Option a, Result a b, ...
        let mut applied = Type::Con(type_name.clone());
        for &p in params {
            applied = Type::App(Box::new(applied), Box::new(Type::Var(p)));
        }

        // コンストラクタの型を構築
        let ctor_type = match self {
            Variant::Unit(_) => applied,
            Variant::Tuple(_, elems) => {
                let mut t = applied;
                for arg in elems.iter().rev() {
                    t = Type::Fun(Box::new(arg.clone()), Box::new(t));
                }
                t
            }
        };

        TypeScheme::poly(params.to_vec(), ctor_type)
    }
}
