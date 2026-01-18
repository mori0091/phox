use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;

use super::*;

// ----------------------------------------------
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NamedStarlet {
    pub name: Symbol,
    pub expr: Expr,
}

// ----------------------------------------------
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypedStarlet {
    pub name: Symbol,
    pub expr: Expr,
    pub ty: Type,
}

// ----------------------------------------------
// FreeTypeVars
impl FreeTypeVars for TypedStarlet {
    fn free_type_vars(&self, ctx: &mut TypeContext, acc: &mut HashSet<TypeVarId>) {
        self.expr.free_type_vars(ctx, acc);
        self.ty.free_type_vars(ctx, acc);
    }
}

// ----------------------------------------------
// Repr
impl Repr for TypedStarlet {
    fn repr(&self, ctx: &mut TypeContext) -> Self {
        let symbol = self.name.clone();
        let expr = self.expr.repr(ctx);
        let ty = self.ty.repr(ctx);
        TypedStarlet { name: symbol, expr, ty }
    }
}

// ----------------------------------------------
// ApplySubst
impl ApplySubst for TypedStarlet {
    fn apply_subst(&self, subst: &Subst) -> Self {
        let symbol = self.name.clone();
        let expr = self.expr.apply_subst(subst);
        let ty = self.ty.apply_subst(subst);
        TypedStarlet { name: symbol, expr, ty }
    }
}

// ----------------------------------------------
// SchemePretty
impl SchemePretty for TypedStarlet {
    fn rename_type_var(&self, map: &mut HashMap<TypeVarId, String>) -> Self {
        let symbol = self.name.clone();
        let expr = self.expr.rename_type_var(map);
        let ty = self.ty.rename_type_var(map);
        TypedStarlet { name: symbol, expr, ty }
    }
}

// ----------------------------------------------
// Pretty
impl Pretty for TypedStarlet {
    fn pretty(&self) -> String {
        self.rename_type_var(&mut HashMap::new()).to_string()
    }
}

// ----------------------------------------------
// fmt::Display
impl fmt::Display for TypedStarlet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "*let {} : {};", self.name.pretty(), self.ty)?;
        writeln!(f, "*let {} = {};", self.name.pretty(), self.expr)
    }
}

// ----------------------------------------------
// Pretty
// impl Pretty for Scheme<TypedStarlet> {
//     fn pretty(&self) -> String {
impl Scheme<TypedStarlet> {
    pub fn pretty(&self) -> String {
        let (head, _) = self.pretty_format();
        head
    }

    pub fn pretty_all(&self) -> String {
        let (head, body) = self.pretty_format();
        format!("{};\n{}", head, body)
    }

    pub fn pretty_format(&self) -> (String, String) {
        let (vars, requires, typed_starlet) = self.pretty_componets();
        let vars =
            if vars.is_empty() {
                "".to_string()
            } else {
                format!("∀ {}. ", vars.join(" "))
            }
        ;

        let name = typed_starlet.name.pretty();
        let head = if requires.is_empty() {
            format!("*let {} : {}{}", name, vars, typed_starlet.ty.pretty())
        }
        else {
            format!("*let {} : {}{} {}", name, vars, typed_starlet.ty.pretty(), requires.pretty())
        };

        let body = format!("*let {} = {}", name, typed_starlet.expr);

        (head, body)
    }

    pub fn pretty_componets(&self) -> (Vec<String>, ConstraintSet, TypedStarlet) {
        // 量化変数に a, b, c... を割り当てる
        let mut map = HashMap::new();
        for (i, v) in self.vars.iter().enumerate() {
            let ch = (b'a' + i as u8) as char;
            map.insert(*v, ch.to_string());
        }

        let requires = self.constraints.rename_type_var(&mut map);
        let typed_starlet = self.target.rename_type_var(&mut map);

        let mut vars: Vec<String> = map.into_values().collect();
        vars.sort();

        (vars, requires, typed_starlet)
    }
}
