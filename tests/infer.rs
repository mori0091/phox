use algo_j::api::{check, check_scheme};
use algo_j::syntax::ast::Type;

#[test]
fn test_lit_unit() {
    assert_eq!(check("()"), Ok(Type::con("()")));
}

#[test]
fn test_lit_true() {
    assert_eq!(check("true"), Ok(Type::con("Bool")));
}

#[test]
fn test_lit_false() {
    assert_eq!(check("false"), Ok(Type::con("Bool")));
}

#[test]
fn test_lit_int() {
    assert_eq!(check("123"), Ok(Type::con("Int")));
}

#[test]
fn test_simple_let() {
    assert_eq!(check("let x = 5 in x + 1"), Ok(Type::con("Int")));
}

#[test]
fn test_var() {
    assert_eq!(check("foo"), Err("infer error: \"unbound variable: foo\"".into()));
}

#[test]
fn test_id() {
    let sch = check_scheme("\\x. x").unwrap();
    assert_eq!(sch.pretty(), "∀a. a -> a");
}

#[test]
fn test_const() {
    let sch = check_scheme("\\x. \\y. x").unwrap();
    assert_eq!(sch.pretty(), "∀a b. a -> b -> a");
}

#[test]
fn test_compose() {
    let sch = check_scheme("\\f. \\g. \\x. f (g x)").unwrap();
    assert_eq!(sch.pretty(), "∀a b c. (b -> c) -> (a -> b) -> a -> c");
}

#[test]
fn test_fun() {
    let sch = check_scheme("\\f. \\x. f x").unwrap();
    assert_eq!(sch.pretty(), "∀a b. (a -> b) -> a -> b");
}

#[test]
fn test_list_id() {
    let sch = check_scheme("\\x. Cons x Nil").unwrap();
    assert_eq!(sch.pretty(), "∀a. a -> List a");
}

#[test]
fn test_option() {
    let sch = check_scheme("\\x. Some x").unwrap();
    assert_eq!(sch.pretty(), "∀a. a -> Option a");
}

#[test]
fn test_if() {
    let sch = check_scheme("if true then 1 else 2").unwrap();
    assert_eq!(sch.pretty(), "Int");
}

#[test]
fn test_tuple() {
    let sch = check_scheme("((), true, 1)").unwrap();
    assert_eq!(sch.pretty(), "((), Bool, Int)");
}
