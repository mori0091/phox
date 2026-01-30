use phox::api::*;

#[test]
fn test_lambda_var_pattern_identity() {
    // f = \x. x
    let (val, sch) = eval(
        "let f = \\x. x;
         f 123"
    ).unwrap();
    assert_eq!(format!("{}", val), "123");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_lambda_tuple_pattern() {
    // f = \(x, y). x
    let (val, sch) = eval(
        "let f = \\(x, y). x;
         f (1, true)"
    ).unwrap();
    assert_eq!(format!("{}", val), "1");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_lambda_record_pattern() {
    // f = \@{x, y}. y
    let (val, sch) = eval(
        "let f = \\@{ x, y }. y;
         f @{ x = 10, y = true }"
    ).unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_lambda_nested_pattern() {
    // f = \((x, y), z). y
    let (val, sch) = eval(
        "let f = \\((x, y), z). y;
         f ((1, 2), 3)"
    ).unwrap();
    assert_eq!(format!("{}", val), "2");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_lambda_constructor_pattern() {
    // Option a = Some a | None
    // f = \Some x. x
    let (val, sch) = eval(
        "type Option a = Some a | None;
         let f = \\Some x. x;
         f (Some 42)"
    ).unwrap();
    assert_eq!(format!("{}", val), "42");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_lambda_curried_add() {
    // add = \x. \y. x + y
    let (val, sch) = eval(
        "let add = \\x. \\y. x + y;
         add 2 3"
    ).unwrap();
    assert_eq!(format!("{}", val), "5");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_lambda_curried_identity_application() {
    // id = \x. x
    // f = \x. \y. x
    let (val, sch) = eval(
        "let id = \\x. x;
         let f = \\x. \\y. x;
         f (id 42) true"
    ).unwrap();
    assert_eq!(format!("{}", val), "42");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_lambda_curried_tuple() {
    // f = \x. \y. (x, y)
    let (val, sch) = eval(
        "let f = \\x. \\y. (x, y);
         f 1 true"
    ).unwrap();
    assert_eq!(format!("{}", val), "(1, true)");
    assert_eq!(format!("{}", sch.pretty()), "(Int, Bool)");
}

#[test]
fn test_lambda_curried_three_args() {
    // f = \x. \y. \z. x + y + z
    let (val, sch) = eval(
        "let f = \\x. \\y. \\z. x + y + z;
         f 1 2 3"
    ).unwrap();
    assert_eq!(format!("{}", val), "6");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_lambda_partial_application() {
    // add = \x. \y. x + y
    // inc = add 1
    let (val, sch) = eval(
        "let add = \\x. \\y. x + y;
         let inc = add 1;
         inc 41"
    ).unwrap();
    assert_eq!(format!("{}", val), "42");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_polymorphic_identity_reuse() {
    // id = \x. x
    let (val, sch) = eval(
        "let id = \\x. x;
         (id 1, id true)"
    ).unwrap();
    assert_eq!(format!("{}", val), "(1, true)");
    assert_eq!(format!("{}", sch.pretty()), "(Int, Bool)");
}

#[test]
fn test_partial_application_polymorphic() {
    // const = \x. \y. x
    let (val, sch) = eval(
        "let const = \\x. \\y. x;
         let k = const 42;
         (k true, k 99)"
    ).unwrap();
    assert_eq!(format!("{}", val), "(42, 42)");
    assert_eq!(format!("{}", sch.pretty()), "(Int, Int)");
}

#[test]
fn test_let_polymorphism_with_partial_application() {
    // pair = \x. \y. (x, y)
    let (val, sch) = eval(
        "let pair = \\x. \\y. (x, y);
         let p1 = pair 1;
         let p2 = pair true;
         (p1 2, p2 false)"
    ).unwrap();
    assert_eq!(format!("{}", val), "((1, 2), (true, false))");
    assert_eq!(format!("{}", sch.pretty()), "((Int, Int), (Bool, Bool))");
}

// -----------------------

#[test]
fn test_type_error_add_int_and_bool() {
    let err = eval("1 + true").unwrap_err();
    assert!(err.contains("type mismatch"), "unexpected error: {err}");
}

#[test]
fn test_pattern_tuple_length_mismatch() {
    let err = eval("let (x, y) = (1, 2, 3);").unwrap_err();
    assert!(err.contains("tuple length mismatch"), "unexpected error: {err}");
}

#[test]
fn test_unbound_variable() {
    let err = eval("x + 1").unwrap_err();
    assert!(err.contains("unbound variable"), "unexpected error: {err}");
}

#[test]
fn test_constructor_arity_mismatch() {
    let err = eval(
        "type Pair a b = Pair (a, b);
         let f = \\Pair x y. x;"  // Pair は1引数なのに2引数でパターンマッチ
    ).unwrap_err();
    assert!(err.contains("arity mismatch for constructor `Pair`"), "unexpected error: {err}");
}

#[test]
fn test_unknown_field_access() {
    let err = eval(
        "let r = @{ x = 1 };
         r.y"
    ).unwrap_err();
    assert!(err.contains("unknown field `y`"), "unexpected error: {err}");
}

#[test]
fn test_unknown_field_in_pattern() {
    let err = eval(
        "let @{ x, y } = @{ x = 1 };"
    ).unwrap_err();
    assert!(err.contains("type mismatch"), "unexpected error: {err}");
}
