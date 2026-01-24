use super::*;

// -------------------------------------------------------------
// === type decl ===
use crate::interpreter::make_constructor;

pub fn resolve_decl_type_def(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    raw: &RawTypeDef,
) -> Result<(), Error> {
    let TypeDef::SumType {
        name: ty_ctor_name,
        params: ty_ctor_params,
        variants,
    } = &resolve_raw_type_def(phox, module, symbol_env, raw)?;

    let icx = &mut phox.get_infer_ctx(module);
    let env = &mut phox.get_value_env(module);

    // kind を構築
    let mut kind = Kind::Type;
    for _ in ty_ctor_params.iter().rev() {
        kind = Kind::Fun(Box::new(Kind::Type), Box::new(kind));
    }
    icx.put_kind(ty_ctor_name.clone(), kind);

    // 各コンストラクタを登録
    for v in variants {
        // Type scheme of the data constructor `v`.
        icx.put_type_scheme(
            v.name(),
            v.as_scheme(ty_ctor_name, ty_ctor_params)
        );
        // Value of the data constructor `v`.
        env.insert(
            v.name(),
            make_constructor(&v.name(), v.arity())
        );
    }

    Ok(())
}

fn resolve_raw_type_def(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    raw: &RawTypeDef,
) -> Result<TypeDef, Error> {
    match raw {
        RawTypeDef::SumType { name, params, variants } => {
            let mut param_map = HashMap::new();
            let mut param_ids = Vec::new();
            for p in params {
                let id = phox.ctx.ty.fresh_var_id();
                param_map.insert(p.clone(), id);
                param_ids.push(id);
            }

            let mut resolved_variants = Vec::new();
            for v in variants.into_iter() {
                let rv = resolve_raw_variant(phox, module, symbol_env, v, &param_map)?;
                resolved_variants.push(rv);
            }
            let symbol = make_symbol(phox, module, symbol_env, &name)?;
            Ok(TypeDef::SumType {
                name: symbol,
                params: param_ids,
                variants: resolved_variants,
            })
        }
    }
}

/// RawVariant を解決して Variant に変換する
fn resolve_raw_variant(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    raw: &RawVariant,
    param_map: &HashMap<String, TypeVarId>,
) -> Result<Variant, Error> {
    let v = match raw {
        RawVariant::Unit(name) => {
            let symbol = make_symbol(phox, module, symbol_env, &name)?;
            Variant::Unit(symbol)
        }
        RawVariant::Tuple(name, elems) => {
            let symbol = make_symbol(phox, module, symbol_env, &name)?;
            let mut elems2 = Vec::new();
            for t in elems.iter() {
                let ty = resolve_raw_type(phox, module, symbol_env, t, &mut param_map.clone())?;
                elems2.push(ty);
            }
            Variant::Tuple(symbol, elems2)
        }
    };
    Ok(v)
}

/// RawType を解決して Type に変換する
pub fn resolve_raw_type(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    raw: &RawType,
    param_map: &mut HashMap<String, TypeVarId>,
) -> Result<Type, Error> {
    let ty = match raw {
        RawType::VarName(name) => {
            let id = param_map
                .entry(name.to_string())
                .or_insert_with(|| phox.ctx.ty.fresh_var_id());
            Type::Var(*id)
        }
        RawType::ConName(symbol) => {
            let mut symbol = symbol.clone();
            resolve_symbol(phox, module, symbol_env, &mut symbol)?;
            Type::Con(symbol)
        }
        RawType::App(f, x) => {
            let f2 = resolve_raw_type(phox, module, symbol_env, f, param_map)?;
            let x2 = resolve_raw_type(phox, module, symbol_env, x, param_map)?;
            Type::App(Box::new(f2), Box::new(x2))
        }
        RawType::Fun(l, r) => {
            let l2 = resolve_raw_type(phox, module, symbol_env, l, param_map)?;
            let r2 = resolve_raw_type(phox, module, symbol_env, r, param_map)?;
            Type::Fun(Box::new(l2), Box::new(r2))
        }
        RawType::Tuple(elems) => {
            let mut elems2 = Vec::new();
            for t in elems.iter() {
                let ty = resolve_raw_type(phox, module, symbol_env, t, param_map)?;
                elems2.push(ty);
            }
            Type::Tuple(elems2)
        }
        RawType::Record(fields) => {
            let mut fields2 = Vec::new();
            for (fname, t) in fields.iter() {
                let ty = resolve_raw_type(phox, module, symbol_env, t, param_map)?;
                fields2.push((fname.clone(), ty));
            }
            Type::Record(fields2)
        }
    };
    Ok(ty)
}
