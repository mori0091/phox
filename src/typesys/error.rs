use crate::module::*;
use crate::syntax::ast::*;
use super::*;

// ===== Type error =====
#[derive(Debug)]
pub enum TypeError {
    // ---- from `resole_*`

    Expeted { expected: String, actual: String },

    UnknownPath(Path),
    UnknownTrait(Symbol),
    UnknownTraitMember(String),
    ConflictImpl { it: TraitHead, other: TraitHead },
    ConflictAlias { name: String, other: Path },
    ArityMismatch { trait_name: Symbol, member: String, expected: usize, actual: usize },
    UnificationFail { expected: TraitHead, actual: TraitHead },

    // ----from `apply_trait_impls_*`

    MissingType,
    MissingTraitImpl(TraitHead),
    MissingTraitMemberImpl(String),
    // MissingTraitImplForMember: "no trait impl for member: eq with type Bool -> Bool -> Bool; expected: Eq Bool"
    MissingTraitImplForMember { member: String, ty: Type, expected: Vec<TraitHead> },
    // AmbiguousTraitMember: "ambiguous trait member: f for type Int; candidates: Foo, Bar"
    AmbiguousTraitMember { member: String, ty: Type, candidates: Vec<String> },
    AmbiguousTrait { trait_head: String, candidates: Vec<String> },

    // ---- from `infer_*`

    Mismatch(Type, Type),
    NoMatchingOverload,
    RecursiveType,
    UnboundVariable(Symbol),
    AmbiguousVariable { name: Symbol, candidates: Vec<TypeScheme> },

    UnknownConstructor(Symbol),
    ConstructorArityMismatch(Symbol, usize, Type),

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
            TypeError::UnknownPath(path) => {
                write!(f, "couldn't resolve path `{}`", path.pretty())
            }
            TypeError::ConflictAlias { name, other } => {
                write!(f, "name `{}` is already used as `{}`", name, other.pretty())
            }
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
                    .map(|sch| format!("@{{{}}}.{}", sch.constraints[0].pretty(), name.pretty()))
                    .collect();
                hints.sort();
                writeln!(f, "ambiguous variable `{}`", name.pretty())?;
                writeln!(f, "candidates:\n  {}", cands.join("\n  "))?;
                write!(f, "solution:\n  {}", hints.join("\n  "))
            }
             _ => write!(f, "{:?}", self),
        }
    }
}
