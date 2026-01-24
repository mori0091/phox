use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use super::*;

// ===== TypeExpr =====
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeExpr {
    Ty(Type),
    // Row(Row),
    // Nat(Nat),
}

// pub fn kind_of(texpr: &TypeExpr) -> Kind {
//     match texpr {
//         TypeExpr::Ty(ty) => match ty {
//             Type::Var(_) => {
//                 todo!()
//             }
//             Type::Con(_name) => {
//                 todo!()
//             }
//             Type::App(ref t1, ref t2) => {
//                 Kind::Fun(Box::new(kind_of(t1)), Box::new(kind_of(t2)))
//             }
//             Type::Fun(_, _) | Type::Tuple(_) | Type::Record(_) => {
//                 Kind::Type
//             }
//         }
//     }
// }

impl TypeExpr {
    pub fn contains_type_var(&self) -> bool {
        match self {
            Self::Ty(ty) => ty.contains_type_var()
        }
    }

    pub fn score(&self) -> (usize, i64) {
        match self {
            Self::Ty(ty) => ty.score()
        }
    }

    pub fn as_ref_type(&self) -> &Type {
        match self {
            Self::Ty(ty) => ty
        }
    }
}

impl ApplySubst for TypeExpr {
    fn apply_subst(&self, subst: &Subst) -> Self {
        match self {
            Self::Ty(ty) => Self::Ty(ty.apply_subst(subst))
        }
    }
}

impl FreeVars for TypeExpr {
    fn free_vars(&self, ctx: &mut TypeContext, acc: &mut HashSet<Var>) {
        match self {
            Self::Ty(ty) => ty.free_vars(ctx, acc),
        }
    }
}

impl Repr for TypeExpr {
    fn repr(&self, ctx: &mut TypeContext) -> Self {
        match self {
            Self::Ty(ty) => Self::Ty(ty.repr(ctx))
        }
    }
}

impl RenameForPretty for TypeExpr {
    fn rename_var(&self, map: &mut HashMap<Var, String>) -> Self {
        match self {
            Self::Ty(ty) => Self::Ty(ty.rename_var(map))
        }
    }
}

impl fmt::Display for TypeExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ty(ty) => write!(f, "{}", ty)
        }
    }
}
