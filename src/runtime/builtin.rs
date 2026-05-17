#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Builtin {
    // === cast operators ===
    // --- u8 -> a ---
    CastU8toI64,
    // CastU8toU8,
    CastU8toU32,

    // --- u32 -> a ---
    CastU32toI64,
    CastU32toU8,
    // CastU32toU32,

    // --- i64 -> a ---
    // CastI64toI64,
    CastI64toU8,
    CastI64toU32,

    // === unary operators ===
    // --- i64 ---
    I64Neg,

    // === binary operators ===
    U8Eq,  U32Eq,  I64Eq,
    U8Neq, U32Neq, I64Neq,

    U8Lt,  U32Lt,  I64Lt,
    U8Le,  U32Le,  I64Le,
    U8Gt,  U32Gt,  I64Gt,
    U8Ge,  U32Ge,  I64Ge,

    U8Add, U32Add, I64Add,
    U8Sub, U32Sub, I64Sub,
    U8Mul, U32Mul, I64Mul,
    U8Div, U32Div, I64Div,
    U8Mod, U32Mod, I64Mod,

    // === arrays ===
    Len,
    Slice,
    Push,
}

pub const ALL_BUILTINS: &[Builtin] = &[
    // === cast operators ===
    // --- u8 -> a ---
    Builtin::CastU8toI64,
    // Builtin::CastU8toU8,
    Builtin::CastU8toU32,

    // --- u32 -> a ---
    Builtin::CastU32toI64,
    Builtin::CastU32toU8,
    // Builtin::CastU32toU32,

    // --- i64 -> a ---
    // Builtin::CastI64toI64,
    Builtin::CastI64toU8,
    Builtin::CastI64toU32,

    // === unary operators ===
    // --- i64 ---
    Builtin::I64Neg,

    // === binary operators ===
    Builtin::U8Eq,  Builtin::U32Eq,  Builtin::I64Eq,
    Builtin::U8Neq, Builtin::U32Neq, Builtin::I64Neq,

    Builtin::U8Lt,  Builtin::U32Lt,  Builtin::I64Lt,
    Builtin::U8Le,  Builtin::U32Le,  Builtin::I64Le,
    Builtin::U8Gt,  Builtin::U32Gt,  Builtin::I64Gt,
    Builtin::U8Ge,  Builtin::U32Ge,  Builtin::I64Ge,

    Builtin::U8Add, Builtin::U32Add, Builtin::I64Add,
    Builtin::U8Sub, Builtin::U32Sub, Builtin::I64Sub,
    Builtin::U8Mul, Builtin::U32Mul, Builtin::I64Mul,
    Builtin::U8Div, Builtin::U32Div, Builtin::I64Div,
    Builtin::U8Mod, Builtin::U32Mod, Builtin::I64Mod,

    // === arrays ===
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
            Builtin::CastU8toI64  => ("__cast_u8_i64__", 1, "u8 -> Int"),
            Builtin::CastU8toU32  => ("__cast_u8_u32__", 1, "u8 -> u32"),
            // --- u32 -> a ---
            Builtin::CastU32toI64 => ("__cast_u32_i64__", 1, "u32 -> Int"),
            Builtin::CastU32toU8  => ("__cast_u32_u8__" , 1, "u32 -> u8"),
            // --- i64 -> a ---
            Builtin::CastI64toU8  => ("__cast_i64_u8__" , 1, "Int -> u8"),
            Builtin::CastI64toU32 => ("__cast_i64_u32__", 1, "Int -> u32"),

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

            // --- u32 ---
            Builtin::U32Eq  => ("__u32_eq__", 2, "u32 -> u32 -> Bool"),
            Builtin::U32Neq => ("__u32_ne__", 2, "u32 -> u32 -> Bool"),

            Builtin::U32Lt  => ("__u32_lt__", 2, "u32 -> u32 -> Bool"),
            Builtin::U32Le  => ("__u32_le__", 2, "u32 -> u32 -> Bool"),
            Builtin::U32Gt  => ("__u32_gt__", 2, "u32 -> u32 -> Bool"),
            Builtin::U32Ge  => ("__u32_ge__", 2, "u32 -> u32 -> Bool"),

            Builtin::U32Add => ("__u32_add__", 2, "u32 -> u32 -> u32"),
            Builtin::U32Sub => ("__u32_sub__", 2, "u32 -> u32 -> u32"),
            Builtin::U32Mul => ("__u32_mul__", 2, "u32 -> u32 -> u32"),
            Builtin::U32Div => ("__u32_div__", 2, "u32 -> u32 -> u32"),
            Builtin::U32Mod => ("__u32_mod__", 2, "u32 -> u32 -> u32"),

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
