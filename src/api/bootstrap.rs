use crate::api::*;
use crate::runtime::*;

fn builtin_func(f: Builtin) -> Value {
    Value::Closure {
        pat: Pat::unresolved_var("a"),
        body: Box::new(Expr::app(Expr::builtin(f), Expr::unresolved_var("a"))),
        env: ValueEnv::new()
    }
}

pub fn bootstrap(phox: &mut PhoxEngine, module: &RefModule) -> Result<(), Error> {
    add_primitive_type(phox, module, "()")?;
    add_primitive_type(phox, module, "Bool")?;
    add_primitive_type(phox, module, "Int")?;

    add_primitive_func(
        phox,
        module,
        "__i64_eq__",
        builtin_func(Builtin::I64Eq),
        builtin_type(Builtin::I64Eq),
    )?;

    add_primitive_func(
        phox,
        module,
        "__i64_ne__",
        builtin_func(Builtin::I64Neq),
        builtin_type(Builtin::I64Neq),
    )?;

    add_primitive_func(
        phox,
        module,
        "__i64_le__",
        builtin_func(Builtin::I64Le),
        builtin_type(Builtin::I64Le),
    )?;

    add_primitive_func(
        phox,
        module,
        "__i64_lt__",
        builtin_func(Builtin::I64Lt),
        builtin_type(Builtin::I64Lt),
    )?;

    add_primitive_func(
        phox,
        module,
        "__i64_ge__",
        builtin_func(Builtin::I64Ge),
        builtin_type(Builtin::I64Ge),
    )?;

    add_primitive_func(
        phox,
        module,
        "__i64_gt__",
        builtin_func(Builtin::I64Gt),
        builtin_type(Builtin::I64Gt),
    )?;

    add_primitive_func(
        phox,
        module,
        "__i64_add__",
        builtin_func(Builtin::I64Add),
        builtin_type(Builtin::I64Add),
    )?;

    add_primitive_func(
        phox,
        module,
        "__i64_sub__",
        builtin_func(Builtin::I64Sub),
        builtin_type(Builtin::I64Sub),
    )?;

    add_primitive_func(
        phox,
        module,
        "__i64_mul__",
        builtin_func(Builtin::I64Mul),
        builtin_type(Builtin::I64Mul),
    )?;

    add_primitive_func(
        phox,
        module,
        "__i64_div__",
        builtin_func(Builtin::I64Div),
        builtin_type(Builtin::I64Div),
    )?;

    add_primitive_func(
        phox,
        module,
        "__i64_mod__",
        builtin_func(Builtin::I64Mod),
        builtin_type(Builtin::I64Mod),
    )?;

    add_primitive_func(
        phox,
        module,
        "__i64_neg__",
        builtin_func(Builtin::I64Neg),
        builtin_type(Builtin::I64Neg),
    )?;

    Ok(())
}

fn add_primitive_type(phox: &mut PhoxEngine, module: &RefModule, name: &str) -> Result<(), Error> {
    let symbol = make_top_level_symbol(phox, module, name)?;
    phox.get_infer_ctx(module).put_kind(symbol, Kind::Type);
    Ok(())
}

fn add_primitive_func(phox: &mut PhoxEngine, module: &RefModule, name: &str, val: Value, ty: Type) -> Result<(), Error> {
    let symbol = make_top_level_symbol(phox, module, name)?;
    phox.get_value_env(module).insert(symbol.clone(), val);
    let icx = &mut phox.get_infer_ctx(module);
    let sch = generalize(&mut phox.ctx, icx, &ty);
    icx.put_type_scheme(symbol, sch);
    Ok(())
}
