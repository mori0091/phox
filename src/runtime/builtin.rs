#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Builtin {
    // BoolNot,
    I64Neg,

    I64Eq,
    I64Neq,

    I64Lt,
    I64Le,
    I64Gt,
    I64Ge,

    I64Add,
    I64Sub,
    I64Mul,
    I64Div,
    I64Mod,
}

use crate::typesys::Type;

pub fn builtin_type(f: Builtin) -> Type {
    match f {
        // Builtin::BoolNot => Type::fun(Type::bool_(), Type::bool_()),
        Builtin::I64Neg  => Type::fun(Type::int(), Type::int()),

        Builtin::I64Eq  => Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::bool_()),
        Builtin::I64Neq => Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::bool_()),

        Builtin::I64Lt  => Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::bool_()),
        Builtin::I64Le  => Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::bool_()),
        Builtin::I64Gt  => Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::bool_()),
        Builtin::I64Ge  => Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::bool_()),

        Builtin::I64Add => Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::int()),
        Builtin::I64Sub => Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::int()),
        Builtin::I64Mul => Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::int()),
        Builtin::I64Div => Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::int()),
        Builtin::I64Mod => Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::int()),
    }
}
