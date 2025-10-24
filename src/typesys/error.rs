use crate::syntax::ast::Pat;
use super::{Type, Constraint, Scheme};

// ===== Type error =====
#[derive(Debug)]
pub enum TypeError {
    // ---- from `resole_*`

    UnknownTraitMember(String),
    ArityMismatch { trait_name: String, member: String, expected: usize, actual: usize },

    // ----from `apply_trait_impls_*`

    MissingType,
    MissingTraitImpl(Constraint),
    MissingTraitMemberImpl(String),
    // MissingTraitImplForMember: "no trait impl for member: eq with type Bool -> Bool -> Bool; expected: Eq Bool"
    MissingTraitImplForMember { member: String, ty: Type, expected: Vec<Constraint> },
    // AmbiguousTraitMember: "ambiguous trait member: f for type Int; candidates: Foo, Bar"
    AmbiguousTraitMember { member: String, ty: Type, candidates: Vec<String> },

    // ---- from `infer_*`

    Mismatch(Type, Type),
    NoMatchingOverload,
    RecursiveType,
    UnboundVariable(String),
    AmbiguousVariable { name: String, candidates: Vec<Scheme> },

    UnknownConstructor(String),
    ConstructorArityMismatch(String, usize, Type),

    EmptyMatch,
    UnsupportedPattern(Pat),
    LetRecPatternNotSupported(Pat),

    ExpectedTuple(Type),
    ExpectedRecord(Type),

    TupleLengthMismatch(usize, usize),
    IndexOutOfBounds(usize, Type),

    UnknownField(String, Type),
}

use std::fmt;

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // ---- resolve errors
            TypeError::MissingTraitImpl(constraint) => {
                write!(f, "no implementation for {}", constraint)
            }
            // ---- infer errors
            TypeError::AmbiguousVariable { name, candidates } => {
                let mut cands: Vec<_> = candidates
                    .iter().map(|sch| sch.pretty()).collect();
                cands.sort();
                let mut hints: Vec<_> = candidates
                    .iter()
                    .map(|sch| format!("@{{{}}}.{name}", sch.constraints[0].to_string()))
                    .collect();
                hints.sort();
                writeln!(f, "ambiguous variable `{name}`")?;
                writeln!(f, "candidates: {}", cands.join(", "))?;
                write!(f, "hint: use {}", hints.join(" or "))
            }
             _ => write!(f, "{:?}", self),
        }
    }
}
