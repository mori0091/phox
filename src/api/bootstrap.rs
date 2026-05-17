use crate::api::*;
use crate::runtime::*;

pub fn bootstrap(phox: &mut PhoxEngine, module: &RefModule) -> Result<(), Error> {
    add_primitive_type(phox, module, "()")?;
    add_primitive_type(phox, module, "Bool")?;
    add_primitive_type(phox, module, "Int")?;
    add_primitive_type(phox, module, "u8")?;
    add_primitive_type(phox, module, "u32")?;
    add_primitive_tycon1(phox, module, "@[]")?;

    for b in ALL_BUILTINS {
        add_builtin(phox, module, b)?;
    }

    Ok(())
}

fn add_primitive_tycon1(phox: &mut PhoxEngine, module: &RefModule, name: &str) -> Result<(), Error> {
    let symbol = make_top_level_symbol(phox, module, name)?;
    phox.get_infer_ctx(module).put_kind(symbol, Kind::Fun(Box::new(Kind::Type), Box::new(Kind::Type)));
    Ok(())
}

fn add_primitive_type(phox: &mut PhoxEngine, module: &RefModule, name: &str) -> Result<(), Error> {
    let symbol = make_top_level_symbol(phox, module, name)?;
    phox.get_infer_ctx(module).put_kind(symbol, Kind::Type);
    Ok(())
}

fn add_builtin(phox: &mut PhoxEngine, module: &RefModule, b: &Builtin) -> Result<(), Error> {
    let (name, _arity, sig) = b.signature();
    let ty = {
        let raw_ty = &crate::api::parse_type(sig)?;
        let symbol_env = &mut phox.get_symbol_env(module);
        let param_map = &mut TyParMap::new();
        resolve_raw_type(phox, module, symbol_env, raw_ty, param_map)?
    };
    let symbol = make_top_level_symbol(phox, module, name)?;
    let icx = &mut phox.get_infer_ctx(module);
    let sch = generalize(&mut phox.ctx, icx, &ty);
    icx.put_type_scheme(symbol, sch);
    Ok(())
}
