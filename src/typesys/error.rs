use crate::module::*;
use crate::syntax::ast::*;
use super::*;

// ===== Type error =====
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeError {
    Message(String),

    // ---- from `resolve_*`
    UnknownPath(Path),
    UnknownTrait(Symbol),
    UnknownTraitMember(String),

    ConflictImpl { it: TraitHead, other: TraitHead },
    ConflictAlias { name: String, other: Path },

    TraitArityMismatch { trait_name: Symbol, expected: usize, actual: usize },
    UnificationFail { expected: TraitHead, actual: TraitHead },

    // ---- from `infer_*`
    TypeMismatch(Type, Type),
    NoMatchingOverload(Expr),
    RecursiveType,

    UnboundVariable(Symbol),
    AmbiguousVariable { name: Symbol, candidates: Vec<TypeScheme> },

    // ---- from `apply_trait_impls_*`
    MissingType(Symbol),
    MissingImpl(TraitHead),
    AmbiguousTrait { trait_head: TraitHead, candidates: Vec<TraitScheme> },

    // --- data constructor pattern ---
    UnknownConstructor(Symbol),
    ConstructorArityMismatch(Symbol, usize, Type),
    // --- match expr ---
    EmptyMatch,
    // --- let rec ---
    UnsupportedLetRecPattern(Pat),

    // --- tuples ---
    ExpectedTuple(Type),
    TupleLengthMismatch(usize, usize),
    IndexOutOfBounds(usize, Type),

    // --- records ---
    ExpectedRecord(Type),
    UnknownField(String, Type),
}

impl TypeError {
    pub fn contains(&self, s: &str) -> bool {
        self.to_string().contains(s)
    }
}

use std::fmt;

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeError::Message(msg) => {
                write!(f, "{}", msg)
            }

            // ---- resolve errors
            TypeError::UnknownPath(path) => {
                write!(f, "cannot resolve path `{}`", path.pretty())
            }
            TypeError::UnknownTrait(symbol) => {
                write!(f, "unknown trait `{}`", symbol.pretty())
            }
            TypeError::UnknownTraitMember(name) => {
                write!(f, "unknown trait member `{}`", name)
            }
            TypeError::ConflictImpl { it, other } => {
                write!(f, "impl `{}` conflicts with `{}`", it.pretty(), other.pretty())
            }
            TypeError::ConflictAlias { name, other } => {
                write!(f, "name `{}` is already used as `{}`", name, other.pretty())
            }
            TypeError::TraitArityMismatch { trait_name, expected, actual } => {
                write!(f, "arity mismatch `{}`; expected {} arguments, but was {}", trait_name.pretty(), expected, actual)
            }
            TypeError::UnificationFail { expected, actual } => {
                write!(f, "trait mismatch; expected {}, but was {}", expected.pretty(), actual.pretty())
            }

            // ---- infer errors
            TypeError::TypeMismatch(t1, t2) => {
                write!(f, "type mismatch `{}` and `{}`", t1.pretty(), t2.pretty())
            }
            TypeError::NoMatchingOverload(expr) => {
                write!(f, "no matching overload for `{}`", expr)
            }
            TypeError::RecursiveType => {
                write!(f, "recursive type")
            }
            TypeError::UnboundVariable(symbol) => {
                write!(f, "unbound variable `{}`", symbol.pretty())
            }
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
            TypeError::MissingType(symbol) => {
                write!(f, "type not infered yet for `{}`", symbol.pretty())
            }
            TypeError::MissingImpl(head) => {
                write!(f, "no implementation for `{}`", head.pretty())
            }
            TypeError::AmbiguousTrait { trait_head, candidates } => {
                let mut cands: Vec<_> = candidates
                    .iter().map(|sch| sch.pretty()).collect();
                cands.sort();
                let mut hints: Vec<_> = candidates
                    .iter()
                    .map(|sch| format!("@{{{}}}", sch.target.pretty()))
                    .collect();
                hints.sort();
                writeln!(f, "ambiguous impl `{}`", trait_head.pretty())?;
                writeln!(f, "candidates:\n  {}", cands.join("\n  "))?;
                write!(f, "solution:\n  {}", hints.join("\n  "))
            }

            // --- patterns
            TypeError::UnknownConstructor(symbol) => {
                write!(f, "unknown data constructor `{}`", symbol.pretty())
            }
            TypeError::ConstructorArityMismatch(symbol, arity, ty) => {
                write!(f, "arity mismatch for constructor `{}` of type `{}` ({} arguments given, but was less than it)",
                       symbol.pretty(), ty.pretty(), arity)
            }
            TypeError::EmptyMatch => {
                write!(f, "no match arms in `match` expression")
            }
            TypeError::UnsupportedLetRecPattern(pat) => {
                write!(f, "pattern `{}` not supported for `let rec` statements", pat)
            }

            // --- tuples
            TypeError::ExpectedTuple(ty) => {
                write!(f, "expected a tuple but was `{}`", ty.pretty())
            }
            TypeError::TupleLengthMismatch(len1, len2) => {
                write!(f, "tuple length mismatch `{}` and `{}`", len1, len2)
            }
            TypeError::IndexOutOfBounds(index, ty) => {
                write!(f, "tuple index out of bounds `{}.{}`", ty.pretty(), index)
            }

            // --- records
            TypeError::ExpectedRecord(ty) => {
                write!(f, "expected a record but was `{}`", ty.pretty())
            }
            TypeError::UnknownField(name, ty) => {
                write!(f, "unknown field `{}` for `{}`", name, ty.pretty())
            }
        }
    }
}
