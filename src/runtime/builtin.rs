#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Builtin {
    // === cast operators ===
    // --- u8 -> a ---
    CastU8toI64,
    // CastU8toU8,
    CastU8toU16,
    CastU8toU32,
    CastU8toU64,

    // --- u16 -> a ---
    CastU16toI64,
    CastU16toU8,
    // CastU16toU16,
    CastU16toU32,
    CastU16toU64,

    // --- u32 -> a ---
    CastU32toI64,
    CastU32toU8,
    CastU32toU16,
    // CastU32toU32,
    CastU32toU64,

    // --- u64 -> a ---
    CastU64toI64,
    CastU64toU8,
    CastU64toU16,
    CastU64toU32,
    // CastU64toU64,

    // --- i64 -> a ---
    // CastI64toI64,
    CastI64toU8,
    CastI64toU16,
    CastI64toU32,
    CastI64toU64,

    // === unary operators ===
    // --- i64 ---
    I64Neg,

    // === binary operators ===
    U8Eq,  U16Eq,  U32Eq,  U64Eq,  I64Eq,
    U8Neq, U16Neq, U32Neq, U64Neq, I64Neq,

    U8Lt,  U16Lt,  U32Lt,  U64Lt,  I64Lt,
    U8Le,  U16Le,  U32Le,  U64Le,  I64Le,
    U8Gt,  U16Gt,  U32Gt,  U64Gt,  I64Gt,
    U8Ge,  U16Ge,  U32Ge,  U64Ge,  I64Ge,

    U8Add, U16Add, U32Add, U64Add, I64Add,
    U8Sub, U16Sub, U32Sub, U64Sub, I64Sub,
    U8Mul, U16Mul, U32Mul, U64Mul, I64Mul,
    U8Div, U16Div, U32Div, U64Div, I64Div,
    U8Mod, U16Mod, U32Mod, U64Mod, I64Mod,

    // === arrays ===
    Len,
    Slice,
    Push,

    // === i/o ===
    Write,
}

pub const ALL_BUILTINS: &[Builtin] = &[
    // === cast operators ===
    // --- u8 -> a ---
    Builtin::CastU8toI64,
    // Builtin::CastU8toU8,
    Builtin::CastU8toU16,
    Builtin::CastU8toU32,
    Builtin::CastU8toU64,

    // --- u16 -> a ---
    Builtin::CastU16toI64,
    Builtin::CastU16toU8,
    // Builtin::CastU16toU16,
    Builtin::CastU16toU32,
    Builtin::CastU16toU64,

    // --- u32 -> a ---
    Builtin::CastU32toI64,
    Builtin::CastU32toU8,
    Builtin::CastU32toU16,
    // Builtin::CastU32toU32,
    Builtin::CastU32toU64,

    // --- u64 -> a ---
    Builtin::CastU64toI64,
    Builtin::CastU64toU8,
    Builtin::CastU64toU16,
    Builtin::CastU64toU32,
    // Builtin::CastU64toU64,

    // --- i64 -> a ---
    // Builtin::CastI64toI64,
    Builtin::CastI64toU8,
    Builtin::CastI64toU16,
    Builtin::CastI64toU32,
    Builtin::CastI64toU64,

    // === unary operators ===
    // --- i64 ---
    Builtin::I64Neg,

    // === binary operators ===
    Builtin::U8Eq,  Builtin::U16Eq,  Builtin::U32Eq,  Builtin::U64Eq,  Builtin::I64Eq,
    Builtin::U8Neq, Builtin::U16Neq, Builtin::U32Neq, Builtin::U64Neq, Builtin::I64Neq,

    Builtin::U8Lt,  Builtin::U16Lt,  Builtin::U32Lt,  Builtin::U64Lt,  Builtin::I64Lt,
    Builtin::U8Le,  Builtin::U16Le,  Builtin::U32Le,  Builtin::U64Le,  Builtin::I64Le,
    Builtin::U8Gt,  Builtin::U16Gt,  Builtin::U32Gt,  Builtin::U64Gt,  Builtin::I64Gt,
    Builtin::U8Ge,  Builtin::U16Ge,  Builtin::U32Ge,  Builtin::U64Ge,  Builtin::I64Ge,

    Builtin::U8Add, Builtin::U16Add, Builtin::U32Add, Builtin::U64Add, Builtin::I64Add,
    Builtin::U8Sub, Builtin::U16Sub, Builtin::U32Sub, Builtin::U64Sub, Builtin::I64Sub,
    Builtin::U8Mul, Builtin::U16Mul, Builtin::U32Mul, Builtin::U64Mul, Builtin::I64Mul,
    Builtin::U8Div, Builtin::U16Div, Builtin::U32Div, Builtin::U64Div, Builtin::I64Div,
    Builtin::U8Mod, Builtin::U16Mod, Builtin::U32Mod, Builtin::U64Mod, Builtin::I64Mod,

    // === arrays ===
    Builtin::Len,
    Builtin::Slice,
    Builtin::Push,

    // === i/o ===
    Builtin::Write,
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
            Builtin::CastU8toU16  => ("__cast_u8_u16__", 1, "u8 -> u16"),
            Builtin::CastU8toU32  => ("__cast_u8_u32__", 1, "u8 -> u32"),
            Builtin::CastU8toU64  => ("__cast_u8_u64__", 1, "u8 -> u64"),
            // --- u16 -> a ---
            Builtin::CastU16toI64 => ("__cast_u16_i64__", 1, "u16 -> Int"),
            Builtin::CastU16toU8  => ("__cast_u16_u8__" , 1, "u16 -> u8"),
            Builtin::CastU16toU32 => ("__cast_u16_u32__", 1, "u16 -> u32"),
            Builtin::CastU16toU64 => ("__cast_u16_u64__", 1, "u16 -> u64"),
            // --- u32 -> a ---
            Builtin::CastU32toI64 => ("__cast_u32_i64__", 1, "u32 -> Int"),
            Builtin::CastU32toU8  => ("__cast_u32_u8__" , 1, "u32 -> u8"),
            Builtin::CastU32toU16 => ("__cast_u32_u16__", 1, "u32 -> u16"),
            Builtin::CastU32toU64 => ("__cast_u32_u64__", 1, "u32 -> u64"),
            // --- u64 -> a ---
            Builtin::CastU64toI64 => ("__cast_u64_i64__", 1, "u64 -> Int"),
            Builtin::CastU64toU8  => ("__cast_u64_u8__" , 1, "u64 -> u8"),
            Builtin::CastU64toU16 => ("__cast_u64_u16__", 1, "u64 -> u16"),
            Builtin::CastU64toU32 => ("__cast_u64_u32__", 1, "u64 -> u32"),
            // --- i64 -> a ---
            Builtin::CastI64toU8  => ("__cast_i64_u8__" , 1, "Int -> u8"),
            Builtin::CastI64toU16 => ("__cast_i64_u16__", 1, "Int -> u16"),
            Builtin::CastI64toU32 => ("__cast_i64_u32__", 1, "Int -> u32"),
            Builtin::CastI64toU64 => ("__cast_i64_u64__", 1, "Int -> u64"),

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

            // --- u16 ---
            Builtin::U16Eq  => ("__u16_eq__", 2, "u16 -> u16 -> Bool"),
            Builtin::U16Neq => ("__u16_ne__", 2, "u16 -> u16 -> Bool"),

            Builtin::U16Lt  => ("__u16_lt__", 2, "u16 -> u16 -> Bool"),
            Builtin::U16Le  => ("__u16_le__", 2, "u16 -> u16 -> Bool"),
            Builtin::U16Gt  => ("__u16_gt__", 2, "u16 -> u16 -> Bool"),
            Builtin::U16Ge  => ("__u16_ge__", 2, "u16 -> u16 -> Bool"),

            Builtin::U16Add => ("__u16_add__", 2, "u16 -> u16 -> u16"),
            Builtin::U16Sub => ("__u16_sub__", 2, "u16 -> u16 -> u16"),
            Builtin::U16Mul => ("__u16_mul__", 2, "u16 -> u16 -> u16"),
            Builtin::U16Div => ("__u16_div__", 2, "u16 -> u16 -> u16"),
            Builtin::U16Mod => ("__u16_mod__", 2, "u16 -> u16 -> u16"),

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

            // --- u64 ---
            Builtin::U64Eq  => ("__u64_eq__", 2, "u64 -> u64 -> Bool"),
            Builtin::U64Neq => ("__u64_ne__", 2, "u64 -> u64 -> Bool"),

            Builtin::U64Lt  => ("__u64_lt__", 2, "u64 -> u64 -> Bool"),
            Builtin::U64Le  => ("__u64_le__", 2, "u64 -> u64 -> Bool"),
            Builtin::U64Gt  => ("__u64_gt__", 2, "u64 -> u64 -> Bool"),
            Builtin::U64Ge  => ("__u64_ge__", 2, "u64 -> u64 -> Bool"),

            Builtin::U64Add => ("__u64_add__", 2, "u64 -> u64 -> u64"),
            Builtin::U64Sub => ("__u64_sub__", 2, "u64 -> u64 -> u64"),
            Builtin::U64Mul => ("__u64_mul__", 2, "u64 -> u64 -> u64"),
            Builtin::U64Div => ("__u64_div__", 2, "u64 -> u64 -> u64"),
            Builtin::U64Mod => ("__u64_mod__", 2, "u64 -> u64 -> u64"),

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

            // === i/o ===
            Builtin::Write => ("__write__", 2, "Int -> @[u8] -> Int"),
        }
    }
}
