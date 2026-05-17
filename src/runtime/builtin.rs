#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Builtin {
    CastU8toI64,
    CastI64toU8,

    I64Neg,

    U8Eq,  I64Eq,
    U8Neq, I64Neq,

    U8Lt,  I64Lt,
    U8Le,  I64Le,
    U8Gt,  I64Gt,
    U8Ge,  I64Ge,

    U8Add, I64Add,
    U8Sub, I64Sub,
    U8Mul, I64Mul,
    U8Div, I64Div,
    U8Mod, I64Mod,

    Len,
    Slice,
    Push,
}

pub const ALL_BUILTINS: &[Builtin] = &[
    Builtin::CastU8toI64,
    Builtin::CastI64toU8,

    Builtin::I64Neg,

    Builtin::U8Eq,  Builtin::I64Eq,
    Builtin::U8Neq, Builtin::I64Neq,

    Builtin::U8Lt,  Builtin::I64Lt,
    Builtin::U8Le,  Builtin::I64Le,
    Builtin::U8Gt,  Builtin::I64Gt,
    Builtin::U8Ge,  Builtin::I64Ge,

    Builtin::U8Add, Builtin::I64Add,
    Builtin::U8Sub, Builtin::I64Sub,
    Builtin::U8Mul, Builtin::I64Mul,
    Builtin::U8Div, Builtin::I64Div,
    Builtin::U8Mod, Builtin::I64Mod,

    Builtin::Len,
    Builtin::Slice,
    Builtin::Push,
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
            // === cast operators ===
            // --- u8 -> a ---
            Builtin::CastU8toI64 => ("__cast_u8_i64__", 1, "u8 -> Int"),
            // --- i64 -> a ---
            Builtin::CastI64toU8 => ("__cast_i64_u8__", 1, "Int -> u8"),

            // === unary operators ===
            // --- i64 ---
            Builtin::I64Neg => ("__i64_neg__", 1, "Int -> Int"),

            // === binary operators ===
            // --- u8 ---
            Builtin::U8Eq  => ("__u8_eq__", 2, "u8 -> u8 -> Bool"),
            Builtin::U8Neq => ("__u8_ne__", 2, "u8 -> u8 -> Bool"),

            Builtin::U8Lt  => ("__u8_lt__", 2, "u8 -> u8 -> Bool"),
            Builtin::U8Le  => ("__u8_le__", 2, "u8 -> u8 -> Bool"),
            Builtin::U8Gt  => ("__u8_gt__", 2, "u8 -> u8 -> Bool"),
            Builtin::U8Ge  => ("__u8_ge__", 2, "u8 -> u8 -> Bool"),

            Builtin::U8Add => ("__u8_add__", 2, "u8 -> u8 -> u8"),
            Builtin::U8Sub => ("__u8_sub__", 2, "u8 -> u8 -> u8"),
            Builtin::U8Mul => ("__u8_mul__", 2, "u8 -> u8 -> u8"),
            Builtin::U8Div => ("__u8_div__", 2, "u8 -> u8 -> u8"),
            Builtin::U8Mod => ("__u8_mod__", 2, "u8 -> u8 -> u8"),

            // --- i64 ---
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

            // === arrays ===
            Builtin::Len   => ("len"   , 1, "@[a] -> Int"),
            Builtin::Slice => ("slice" , 3, "@[a] -> Int -> Int -> @[a]"),
            Builtin::Push  => ("push"  , 2, "@[a] -> a -> @[a]"),
        }
    }
}
