use super::{Scheme, Constraint, Type, TypeVarId};

pub type TypeScheme = Scheme<Type>;

impl TypeScheme {
    pub fn pretty(&self) -> String {
        use std::collections::HashMap;

        // 量化変数に a, b, c... を割り当てる
        let mut map = HashMap::new();
        for (i, v) in self.vars.iter().enumerate() {
            let ch = (b'a' + i as u8) as char;
            map.insert(*v, ch.to_string());
        }

        fn rename_type_var(ty: &Type, map: &HashMap<TypeVarId, String>) -> Type {
            match ty {
                Type::Var(v) => {
                    if let Some(name) = map.get(v) {
                        Type::Con(name.clone()) // ここでは Var を Con に置き換えてもよい
                    } else {
                        Type::Var(*v) // 自由変数はそのまま
                    }
                }
                Type::Con(name) => Type::Con(name.clone()),
                Type::Fun(t1, t2) => {
                    Type::fun(rename_type_var(t1, map), rename_type_var(t2, map))
                }
                Type::App(t1, t2) => {
                    Type::app(rename_type_var(t1, map), rename_type_var(t2, map))
                }
                Type::Tuple(ts) => {
                    Type::Tuple(ts.iter().map(|t| rename_type_var(t, map)).collect())
                }
                Type::Record(fields) => {
                    Type::Record(
                        fields.iter().map(|(f, t)| (f.clone(), rename_type_var(t, map))).collect()
                    )
                }
                Type::Overloaded(name, cands) => {
                    let mut new_cands = vec![];
                    for sch in cands.iter() {
                        let vars = sch.vars.clone();
                        let constraints = sch.constraints.iter().map(|c| {
                            let params = c.params.iter().map(|t| rename_type_var(t, map)).collect();
                            Constraint { name: c.name.clone(), params }
                        }).collect();
                        let target = rename_type_var(&sch.target, map);
                        new_cands.push(TypeScheme::new(vars, constraints, target));
                    }
                    Type::Overloaded(name.clone(), new_cands)
                }
            }
        }

        let mut renamed = rename_type_var(&self.target, &map).to_string();

        if !self.constraints.is_empty() {
            let cs = self.constraints
                          .iter()
                          .map(|c| c.to_string())
                          .collect::<Vec<_>>()
                .join(", ");
            renamed = format!("{} => {}", cs, renamed);
        }

        if self.vars.is_empty() {
            format!("{}", renamed)
        } else {
            let vars: Vec<String> = (0..self.vars.len())
                .map(|i| ((b'a' + i as u8) as char).to_string())
                .collect();
            format!("∀ {}. {}", vars.join(" "), renamed)
        }
    }
}
