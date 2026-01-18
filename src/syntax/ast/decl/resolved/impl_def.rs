use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;

use super::*;

// ----------------------------------------------
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NamedImpl {
    pub head: TraitHead,
    pub members: Vec<(Symbol, Expr)>,
}

// ----------------------------------------------
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypedImpl {
    pub head: TraitHead,
    pub members: Vec<(Symbol, Expr, Type)>,
}

// ----------------------------------------------
pub type ImplScheme = Scheme<TypedImpl>;

pub type ImplTemplate = SchemeTemplate<TypedImpl>;

// ----------------------------------------------
// FreeTypeVars
impl FreeTypeVars for TypedImpl {
    fn free_type_vars(&self, ctx: &mut TypeContext, acc: &mut HashSet<TypeVarId>) {
        self.head.free_type_vars(ctx, acc);
        for (_sym, e, ty) in self.members.iter() {
            e.free_type_vars(ctx, acc);
            ty.free_type_vars(ctx, acc);
        }
    }
}

// ----------------------------------------------
// Repr
impl Repr for TypedImpl {
    fn repr(&self, ctx: &mut TypeContext) -> Self {
        let head = self.head.repr(ctx);
        let members = self.members.iter().map(|(sym, e, ty)| {
            (sym.clone(), e.repr(ctx), ty.repr(ctx))
        }).collect();
        TypedImpl { head, members }
    }
}

// ----------------------------------------------
// ApplySubst
impl ApplySubst for TypedImpl {
    fn apply_subst(&self, subst: &Subst) -> Self {
        let head = self.head.apply_subst(subst);
        let members = self.members.iter().map(|(sym, e, ty)| {
            (sym.clone(), e.apply_subst(subst), ty.apply_subst(subst))
        }).collect();
        TypedImpl { head, members }
    }
}

// ----------------------------------------------
// SchemePretty
impl SchemePretty for TypedImpl {
    fn rename_type_var(&self, map: &mut HashMap<TypeVarId, String>) -> Self {
        let head = self.head.rename_type_var(map);
        let members = self.members.iter().map(
            |(sym, expr, ty)| (
                sym.clone(),
                expr.rename_type_var(map),
                ty.rename_type_var(map)
            )).collect();
        TypedImpl { head, members }
    }
}

// ----------------------------------------------
// Pretty
impl Pretty for TypedImpl {
    fn pretty(&self) -> String {
        self.rename_type_var(&mut HashMap::new()).to_string()
    }
}

// ----------------------------------------------
// fmt::Display
impl fmt::Display for TypedImpl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "impl {} {{", self.head)?;
        for (sym, expr, ty) in &self.members {
            writeln!(f, "  {} : {};", sym.pretty(), ty)?;
            writeln!(f, "  {} = {};", sym.pretty(), expr)?;
        }
        write!(f, "}}")
    }
}

// ----------------------------------------------
// Pretty
// impl Pretty for Scheme<TypedImpl> {
//     fn pretty(&self) -> String {
impl Scheme<TypedImpl> {
    pub fn pretty(&self) -> String {
        let (head, _) = self.pretty_format();
        head
    }

    pub fn prettry_all(&self) -> String {
        let (head, body) = self.pretty_format();
        format!("{} {}", head, body)
    }

    pub fn pretty_format(&self) -> (String, String) {
        let (vars, requires, typed_impl) = self.pretty_componets();
        let vars =
            if vars.is_empty() {
                "".to_string()
            } else {
                format!("∀ {}. ", vars.join(" "))
            }
        ;

        let head = if requires.is_empty() {
            format!("{}impl {}", vars, typed_impl.head.pretty())
        }
        else {
            format!("{}impl {} {}", vars, typed_impl.head.pretty(), requires.pretty())
        };

        let mut xs = Vec::new();
        xs.push(format!("{{"));
        for (sym, expr, ty) in &typed_impl.members {
            xs.push(format!("  {} : {};", sym.pretty(), ty.pretty()));
            xs.push(format!("  {} = {};", sym.pretty(), expr));
        }
        xs.push(format!("}}"));
        let body = xs.join("\n");

        (head, body)
    }

    pub fn pretty_componets(&self) -> (Vec<String>, ConstraintSet, TypedImpl) {
        // 量化変数に a, b, c... を割り当てる
        let mut map = HashMap::new();
        for (i, v) in self.vars.iter().enumerate() {
            let ch = (b'a' + i as u8) as char;
            map.insert(*v, ch.to_string());
        }

        let requires = self.constraints.rename_type_var(&mut map);
        let typed_impl = self.target.rename_type_var(&mut map);

        let mut vars: Vec<String> = map.into_values().collect();
        vars.sort();

        (vars, requires, typed_impl)
    }
}

impl Scheme<TypedImpl> {
    pub fn get_member_scheme(&self, symbol: &Symbol) -> Option<Scheme<Type>> {
        match self.target.members.iter().find(|(sym, _e, _t)| sym == symbol) {
            Some((_sym, _e, ty)) => {
                let mut sch = Scheme::new(self.vars.clone(), self.constraints.clone(), ty.clone());
                sch.constraints.primary = Some(Box::new(self.target.head.clone()));
                Some(sch)
            }
            None => None,
        }
    }
}
