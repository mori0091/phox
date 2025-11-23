use std::rc::Rc;

use crate::api::PhoxEngine;
use crate::resolve::*;
use crate::syntax::ast::*;
use crate::module::*;
use crate::typesys::*;
use crate::interpreter::*;

pub fn bootstrap(phox: &mut PhoxEngine, module: &RefModule) -> Result<(), TypeError> {
    add_primitive_type(phox, module, "()")?;
    add_primitive_type(phox, module, "Bool")?;
    add_primitive_type(phox, module, "Int")?;

    add_primitive_func(
        phox,
        module,
        "__i64_eq__",
        make_i64_cmp_op(|a, b| a == b),
        Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::bool_()), // (Int, Int) -> Bool
    )?;

    add_primitive_func(
        phox,
        module,
        "__i64_ne__",
        make_i64_cmp_op(|a, b| a != b),
        Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::bool_()), // (Int, Int) -> Bool
    )?;

    add_primitive_func(
        phox,
        module,
        "__i64_le__",
        make_i64_cmp_op(|a, b| a <= b),
        Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::bool_()), // (Int, Int) -> Bool
    )?;

    add_primitive_func(
        phox,
        module,
        "__i64_lt__",
        make_i64_cmp_op(|a, b| a < b),
        Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::bool_()), // (Int, Int) -> Bool
    )?;

    add_primitive_func(
        phox,
        module,
        "__i64_ge__",
        make_i64_cmp_op(|a, b| a >= b),
        Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::bool_()), // (Int, Int) -> Bool
    )?;

    add_primitive_func(
        phox,
        module,
        "__i64_gt__",
        make_i64_cmp_op(|a, b| a > b),
        Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::bool_()), // (Int, Int) -> Bool
    )?;

    add_primitive_func(
        phox,
        module,
        "__i64_add__",
        make_i64_arith_op(|a, b| a + b),
        Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::int()), // (Int, Int) -> Int
    )?;

    add_primitive_func(
        phox,
        module,
        "__i64_sub__",
        make_i64_arith_op(|a, b| a - b),
        Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::int()), // (Int, Int) -> Int
    )?;

    add_primitive_func(
        phox,
        module,
        "__i64_mul__",
        make_i64_arith_op(|a, b| a * b),
        Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::int()), // (Int, Int) -> Int
    )?;

    add_primitive_func(
        phox,
        module,
        "__i64_div__",
        make_i64_arith_op(|a, b| {
            if b == 0 {
                panic!("division by zero");
            }
            a / b
        }),
        Type::fun(Type::Tuple(vec![Type::int(), Type::int()]), Type::int()), // (Int, Int) -> Int
    )?;

    add_primitive_func(
        phox,
        module,
        "__i64_neg__",
        make_i64_unary_op(|x| -x),
        Type::fun(Type::int(), Type::int()),
    )?;

    Ok(())
}

fn add_primitive_type(phox: &mut PhoxEngine, module: &RefModule, name: &str) -> Result<(), TypeError> {
    let symbol = make_top_level_symbol(phox, module, name)?;
    phox.get_infer_ctx(module).put_kind(symbol, Kind::Star);
    Ok(())
}

fn add_primitive_func(phox: &mut PhoxEngine, module: &RefModule, name: &str, val: Value, ty: Type) -> Result<(), TypeError> {
    let symbol = make_top_level_symbol(phox, module, name)?;
    phox.get_value_env(module).insert(
        symbol.clone(),
        val
    );
    phox.get_infer_ctx(module).put_type_scheme(
        symbol,
        TypeScheme {
            vars: vec![],
            constraints: vec![],
            target: ty,
        }
    );
    Ok(())
}

/// 単項の整数演算子をBuiltinとして作る
/// Int -> Int
fn make_i64_unary_op<F>(op: F) -> Value
where
    F: Fn(i64) -> i64 + 'static,
{
    Value::Builtin(Rc::new(move |arg: Value| {
        if let Value::Lit(Lit::Int(a)) = arg {
            return Value::Lit(Lit::Int(op(a)));
        }
        panic!("type error in <builtin>");
    }))
}

/// Int -> Int -> Int
fn make_i64_arith_op<F>(op: F) -> Value
where
    F: Fn(i64, i64) -> i64 + 'static,
{
    Value::Builtin(Rc::new(move |arg: Value| {
        if let Value::Tuple(xs) = arg {
            if let [Value::Lit(Lit::Int(a)), Value::Lit(Lit::Int(b))] = &xs[..] {
                return Value::Lit(Lit::Int(op(*a, *b)));
            }
        }
        panic!("type error in <builtin>");
    }))
}

/// 2引数の比較演算子をBuiltinとして作る
/// Int -> Int -> Bool
fn make_i64_cmp_op<F>(op: F) -> Value
where
    F: Fn(i64, i64) -> bool + 'static,
{
    Value::Builtin(Rc::new(move |arg: Value| {
        if let Value::Tuple(xs) = arg {
            if let [Value::Lit(Lit::Int(a)), Value::Lit(Lit::Int(b))] = &xs[..] {
                return Value::Lit(Lit::Bool(op(*a, *b)));
            }
        }
        panic!("type error in <builtin>");
    }))
}
