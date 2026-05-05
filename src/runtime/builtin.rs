#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Builtin {
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

pub const ALL_BUILTINS: &[Builtin] = &[
    Builtin::I64Neg,

    Builtin::I64Eq,
    Builtin::I64Neq,

    Builtin::I64Lt,
    Builtin::I64Le,
    Builtin::I64Gt,
    Builtin::I64Ge,

    Builtin::I64Add,
    Builtin::I64Sub,
    Builtin::I64Mul,
    Builtin::I64Div,
    Builtin::I64Mod,
];

impl Builtin {
    pub fn name(&self) -> &'static str {
        self.signature().0
    }
    pub fn arity(&self) -> usize {
        self.signature().1
    }
    pub fn signature(&self) -> (&'static str, usize, &'static str) {
        match self {
            Builtin::I64Neg => ("__i64_neg__", 1, "Int -> Int"),

            Builtin::I64Eq  => ("__i64_eq__", 2, "Int -> Int -> Bool"),
            Builtin::I64Neq => ("__i64_ne__", 2, "Int -> Int -> Bool"),

            Builtin::I64Lt  => ("__i64_lt__", 2, "Int -> Int -> Bool"),
            Builtin::I64Le  => ("__i64_le__", 2, "Int -> Int -> Bool"),
            Builtin::I64Gt  => ("__i64_gt__", 2, "Int -> Int -> Bool"),
            Builtin::I64Ge  => ("__i64_ge__", 2, "Int -> Int -> Bool"),

            Builtin::I64Add => ("__i64_add__", 2, "Int -> Int -> Int"),
            Builtin::I64Sub => ("__i64_sub__", 2, "Int -> Int -> Int"),
            Builtin::I64Mul => ("__i64_mul__", 2, "Int -> Int -> Int"),
            Builtin::I64Div => ("__i64_div__", 2, "Int -> Int -> Int"),
            Builtin::I64Mod => ("__i64_mod__", 2, "Int -> Int -> Int"),
        }
    }
}
