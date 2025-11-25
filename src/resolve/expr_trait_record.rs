use super::*;

pub fn resolve_expr_trait_record(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    raw: &RawTraitHead,
) -> Result<ExprBody, TypeError> {
    let impl_head = resolve_impl_head(phox, module, symbol_env, &raw, &HashMap::new())?;
    let base_score = impl_head.score();
    let mut matches = Vec::new();
    for (impl_sch, member_map) in phox.impl_env.iter() {
        // impl_sch: TraitScheme
        let (_impl_constraints, candidate_impl_head) = impl_sch.instantiate(&mut phox.ctx);

        // impl_head と required trait_head を unify
        if candidate_impl_head.name == impl_head.name && candidate_impl_head.score() == base_score {
            let mut dummy_ctx = phox.ctx.clone();
            if impl_head.unify(&mut dummy_ctx, &candidate_impl_head).is_ok() {
                matches.push((impl_sch, member_map));
            }
        }
    }
    match matches.len() {
        0 => {
            // 実装が見つからない
            Err(TypeError::MissingImpl(impl_head.clone()))
        }
        1 => {
            let (impl_sch, impls) = matches[0];
            let (_impl_constraints, impl_head) = impl_sch.instantiate(&mut phox.ctx);
            impl_head.unify(&mut phox.ctx, &impl_head)?;
            let fields: Vec<(String, Expr)> = impls
                .iter().map(|(k, v)| (k.to_string(), v.clone())).collect();
            Ok(ExprBody::Record(fields))
        }
        _ => {
            let cand_traits: Vec<TraitScheme> =
                matches.into_iter().map(|(trait_sch, _)| trait_sch.clone()).collect();
            Err(TypeError::AmbiguousTrait {
                trait_head: impl_head.clone(),
                candidates: cand_traits,
            })
        }
    }
}
