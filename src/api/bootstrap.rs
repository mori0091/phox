use crate::api::*;
use crate::runtime::*;

pub fn bootstrap(phox: &mut PhoxEngine, module: &RefModule) -> Result<(), Error> {
    add_primitive_type(phox, module, "()")?;
    add_primitive_type(phox, module, "Bool")?;
    add_primitive_type(phox, module, "Int")?;
    add_primitive_tycon1(phox, module, "@[]")?;

    add_builtin(phox, module, "__i64_eq__", Builtin::I64Eq)?;
    add_builtin(phox, module, "__i64_ne__", Builtin::I64Neq)?;
    add_builtin(phox, module, "__i64_le__", Builtin::I64Le)?;
    add_builtin(phox, module, "__i64_lt__", Builtin::I64Lt)?;
    add_builtin(phox, module, "__i64_ge__", Builtin::I64Ge)?;
    add_builtin(phox, module, "__i64_gt__", Builtin::I64Gt)?;
    add_builtin(phox, module, "__i64_add__", Builtin::I64Add)?;
    add_builtin(phox, module, "__i64_sub__", Builtin::I64Sub)?;
    add_builtin(phox, module, "__i64_mul__", Builtin::I64Mul)?;
    add_builtin(phox, module, "__i64_div__", Builtin::I64Div)?;
    add_builtin(phox, module, "__i64_mod__", Builtin::I64Mod)?;
    add_builtin(phox, module, "__i64_neg__", Builtin::I64Neg)?;

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

fn add_builtin(phox: &mut PhoxEngine, module: &RefModule, name: &str, b: Builtin) -> Result<(), Error> {
    let symbol = make_top_level_symbol(phox, module, name)?;
    let icx = &mut phox.get_infer_ctx(module);
    let sch = generalize(&mut phox.ctx, icx, &builtin_type(b));
    icx.put_type_scheme(symbol, sch);
    Ok(())
}
