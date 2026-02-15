use crate::module::*;
use crate::syntax::ast::*;
use crate::typesys::*;

// ===== Type error =====
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Error {
    Message(String),

    // ---- from `resolve_*`
    UnknownPath(Path),
    UnknownTrait(Symbol),
    UnknownTraitMember(String),

    ConflictAlias { name: String, other: Path },

    TraitArityMismatch { trait_name: Symbol, expected: usize, actual: usize },
    UnificationFail { expected: TraitHead, actual: TraitHead },

    // ---- from `infer_*`
    TypeMismatch(Type, Type),
    NoMatchingOverload(Expr),
    RecursiveType,

    UnboundVariable(Symbol),
    AmbiguousVariable { name: Symbol, candidates: Vec<TypeScheme> },

    // ---- from `register`
    ConflictImpl { it: TraitHead },
    ConflictStarlet { it: Scheme<TypedStarlet> },

    // ---- from `apply_trait_impls_*`
    MissingType(Symbol),
    MissingImpl(TraitHead),
    AmbiguousImpl { trait_head: TraitHead, candidates: Vec<TraitScheme> },

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

    // ---- from `eval`
    DivisionByZero,
}

impl Error {
    pub fn contains(&self, s: &str) -> bool {
        self.to_string().contains(s)
    }
}

use std::fmt;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Message(msg) => {
                write!(f, "{}", msg)
            }

            // ---- resolve errors
            Error::UnknownPath(path) => {
                write!(f, "cannot resolve path `{}`", path.pretty())
            }
            Error::UnknownTrait(symbol) => {
                write!(f, "unknown trait `{}`", symbol.pretty())
            }
            Error::UnknownTraitMember(name) => {
                write!(f, "unknown trait member `{}`", name)
            }
            Error::ConflictAlias { name, other } => {
                write!(f, "name `{}` is already used as `{}`", name, other.pretty())
            }
            Error::TraitArityMismatch { trait_name, expected, actual } => {
                write!(f, "arity mismatch `{}`; expected {} arguments, but was {}", trait_name.pretty(), expected, actual)
            }
            Error::UnificationFail { expected, actual } => {
                write!(f, "trait mismatch; expected {}, but was {}", expected.pretty(), actual.pretty())
            }

            // ---- infer errors
            Error::TypeMismatch(t1, t2) => {
                write!(f, "type mismatch `{}` and `{}`", t1.pretty(), t2.pretty())
            }
            Error::NoMatchingOverload(expr) => {
                write!(f, "no matching overload for `{}`", expr)
            }
            Error::RecursiveType => {
                write!(f, "recursive type")
            }
            Error::UnboundVariable(symbol) => {
                write!(f, "unbound variable `{}`", symbol.pretty())
            }
            Error::AmbiguousVariable { name, candidates } => {
                let mut cands: Vec<_> = candidates
                    .iter().map(|sch| sch.pretty()).collect();
                cands.sort();
                let mut hints: Vec<_> = candidates
                    .iter()
                    .map(|sch| {
                        if sch.constraints.primary.is_some() {
                            format!("@{{{}}}.{}", sch.constraints.primary.clone().unwrap().pretty(), name.pretty())
                        }
                        else {
                            format!("{}", sch.pretty())
                        }
                    })
                    .collect();
                hints.sort();
                writeln!(f, "ambiguous variable `{}`", name.pretty())?;
                writeln!(f, "candidates:\n  {}", cands.join("\n  "))?;
                write!(f, "solution:\n  {}", hints.join("\n  "))
            }
            Error::MissingType(symbol) => {
                write!(f, "type not infered yet for `{}`", symbol.pretty())
            }
            Error::MissingImpl(head) => {
                write!(f, "no implementation for `{}`", head.pretty())
            }
            Error::AmbiguousImpl { trait_head, candidates } => {
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

            // ---- register errors
            Error::ConflictImpl { it } => {
                write!(f, "impl `{}` conflicts with previously defined", it.pretty())
            }
            Error::ConflictStarlet { it } => {
                write!(f, "*let `{}` conflicts with previously defined", it.pretty())
            }

            // --- patterns
            Error::UnknownConstructor(symbol) => {
                write!(f, "unknown data constructor `{}`", symbol.pretty())
            }
            Error::ConstructorArityMismatch(symbol, arity, ty) => {
                write!(f, "arity mismatch for constructor `{}` of type `{}` ({} arguments given, but was less than it)",
                       symbol.pretty(), ty.pretty(), arity)
            }
            Error::EmptyMatch => {
                write!(f, "no match arms in `match` expression")
            }
            Error::UnsupportedLetRecPattern(pat) => {
                write!(f, "pattern `{}` not supported for `let rec` statements", pat)
            }

            // --- tuples
            Error::ExpectedTuple(ty) => {
                write!(f, "expected a tuple but was `{}`", ty.pretty())
            }
            Error::TupleLengthMismatch(len1, len2) => {
                write!(f, "tuple length mismatch `{}` and `{}`", len1, len2)
            }
            Error::IndexOutOfBounds(index, ty) => {
                write!(f, "tuple index out of bounds `{}.{}`", ty.pretty(), index)
            }

            // --- records
            Error::ExpectedRecord(ty) => {
                write!(f, "expected a record but was `{}`", ty.pretty())
            }
            Error::UnknownField(name, ty) => {
                write!(f, "unknown field `{}` for `{}`", name, ty.pretty())
            }

            // ---- from `eval`
            Error::DivisionByZero => {
                write!(f, "division by zero")
            }
        }
    }
}
