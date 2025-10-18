use phox::api::{check_expr_type, check_expr_scheme};
use phox::typesys::Type;

#[test]
fn test_lit_unit() {
    assert_eq!(check_expr_type("()"), Ok(Type::con("()")));
}

#[test]
fn test_lit_true() {
    assert_eq!(check_expr_type("true"), Ok(Type::con("Bool")));
}

#[test]
fn test_lit_false() {
    assert_eq!(check_expr_type("false"), Ok(Type::con("Bool")));
}

#[test]
fn test_lit_int() {
    assert_eq!(check_expr_type("123"), Ok(Type::con("Int")));
}

#[test]
fn test_simple_let() {
    assert_eq!(check_expr_type("{ let x = 5 ; x + 1 }"), Ok(Type::con("Int")));
}

#[test]
fn test_var() {
    assert_eq!(check_expr_type("foo"), Err("infer error: UnboundVariable(\"foo\")".into()));
}

#[test]
fn test_id() {
    let sch = check_expr_scheme("\\x. x").unwrap();
    assert_eq!(sch.pretty(), "∀ a. a -> a");
}

#[test]
fn test_const() {
    let sch = check_expr_scheme("\\x. \\y. x").unwrap();
    assert_eq!(sch.pretty(), "∀ a b. a -> b -> a");
}

#[test]
fn test_compose() {
    let sch = check_expr_scheme("\\f. \\g. \\x. f (g x)").unwrap();
    assert_eq!(sch.pretty(), "∀ a b c. (b -> c) -> (a -> b) -> a -> c");
}

#[test]
fn test_fun() {
    let sch = check_expr_scheme("\\f. \\x. f x").unwrap();
    assert_eq!(sch.pretty(), "∀ a b. (a -> b) -> a -> b");
}

#[test]
fn test_list_id() {
    let sch = check_expr_scheme("\\x. Cons x Nil").unwrap();
    assert_eq!(sch.pretty(), "∀ a. a -> List a");
}

#[test]
fn test_option() {
    let sch = check_expr_scheme("\\x. Some x").unwrap();
    assert_eq!(sch.pretty(), "∀ a. a -> Option a");
}

#[test]
fn test_if() {
    let sch = check_expr_scheme("if (true) 1 else 2").unwrap();
    assert_eq!(sch.pretty(), "Int");
}

#[test]
fn test_tuple() {
    let sch = check_expr_scheme("((), true, 1)").unwrap();
    assert_eq!(sch.pretty(), "((), Bool, Int)");
}

#[test]
fn test_singleton_tuple() {
    let sch = check_expr_scheme("(1,)").unwrap();
    assert_eq!(sch.pretty(), "(Int,)");
}

#[test]
fn test_nested_tuple() {
    let sch = check_expr_scheme("((true, 1), ())").unwrap();
    assert_eq!(sch.pretty(), "((Bool, Int), ())");
}

#[test]
fn test_lambda_returns_tuple() {
    let sch = check_expr_scheme("\\x. (x, x)").unwrap();
    assert_eq!(sch.pretty(), "∀ a. a -> (a, a)");
}

#[test]
fn test_tuple_argument_function() {
    let sch = check_expr_scheme("\\t. { let (x, y) = t ; x }").unwrap();
    assert_eq!(sch.pretty(), "∀ a b. (a, b) -> a");
}

#[test]
fn test_tuple_mismatch_length() {
    let err = check_expr_scheme("if (true) (1,) else (1, 2)").unwrap_err();
    assert!(err.contains("TupleLengthMismatch"));
}

#[test]
fn test_tuple_mismatch_type() {
    let err = check_expr_scheme("if (true) (1, true) else (1, 1)").unwrap_err();
    assert!(err.contains("Mismatch"));
}

#[test]
fn test_empty_tuple() {
    let sch = check_expr_scheme("()").unwrap();
    assert_eq!(sch.pretty(), "()");
}

#[test]
fn test_tuple_pattern() {
    let sch = check_expr_scheme("{ let (x, y) = (1, true) ; x }").unwrap();
    assert_eq!(sch.pretty(), "Int");
}

#[test]
fn test_wildcard_pattern() {
    let sch = check_expr_scheme("{ let (_, y) = (1, true) ; y }").unwrap();
    assert_eq!(sch.pretty(), "Bool");
}

#[test]
fn test_literal_pattern() {
    let sch = check_expr_scheme("{ let true = true ; 1 }").unwrap();
    assert_eq!(sch.pretty(), "Int");
}

#[test]
fn test_option_pattern() {
    let sch = check_expr_scheme("{ let Some x = Some 1 ; x }").unwrap();
    assert_eq!(sch.pretty(), "Int");
}

#[test]
fn test_none_pattern() {
    let sch = check_expr_scheme("{ let None = None ; 42 }").unwrap();
    assert_eq!(sch.pretty(), "Int");
}

#[test]
fn test_nil_pattern() {
    let sch = check_expr_scheme("{ let Nil = Nil ; 0 }").unwrap();
    assert_eq!(sch.pretty(), "Int");
}

// --------------

#[test]
fn test_cons_pattern_single() {
    let sch = check_expr_scheme("{ let Cons x xs = Cons 1 Nil ; x }").unwrap();
    assert_eq!(sch.pretty(), "Int");
}

#[test]
fn test_cons_pattern_tail() {
    let sch = check_expr_scheme("{ let Cons x xs = Cons 1 Nil ; xs }").unwrap();
    assert_eq!(sch.pretty(), "List Int");
}

#[test]
fn test_nested_cons_pattern_mismatch() {
    let err = check_expr_scheme("{ let Cons x (Cons y ys) = Cons 1 (Cons true Nil) ; y }").unwrap_err();
    assert!(err.contains("Mismatch"));
}

#[test]
fn test_nested_cons_pattern() {
    let sch = check_expr_scheme("{ let Cons x (Cons y ys) = Cons 1 (Cons 2 Nil) ; y }").unwrap();
    assert_eq!(sch.pretty(), "Int");
}

#[test]
fn test_list_identity_function() {
    let sch = check_expr_scheme("\\t. { let Cons x xs = t ; Cons x xs }").unwrap();
    assert_eq!(sch.pretty(), "∀ a. List a -> List a");
}

#[test]
fn test_seq() {
    let sch = check_expr_scheme("{ let x = 1; let y = 2; x+y }").unwrap();
    assert_eq!(sch.pretty(), "Int");
}
