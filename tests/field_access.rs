use algo_j::api::check_expr_scheme;

#[test]
fn test_field_access_type_simple() {
    let sch = check_expr_scheme("{ let p = @{ x: 42, y: true }; p.x }").unwrap();
    assert_eq!(sch.pretty(), "Int");
}

#[test]
fn test_field_access_type_nested() {
    let sch = check_expr_scheme("{ let p = @{ inner: @{ x: 1 } }; p.inner.x }").unwrap();
    assert_eq!(sch.pretty(), "Int");
}

#[test]
fn test_field_access_type_missing_field() {
    let err = check_expr_scheme("{ let p = @{ x: 42 }; p.y }").unwrap_err();
    assert!(format!("{:?}", err).contains("UnknownField"));
}

#[test]
fn test_field_access_type_non_record() {
    let err = check_expr_scheme("{ let n = 42; n.x }").unwrap_err();
    assert!(format!("{:?}", err).contains("ExpectedRecord"));
}

use algo_j::api::eval_expr;

#[test]
fn test_field_access_simple() {
    let (val, sch) = eval_expr("{ let p = @{ x: 42, y: true }; p.x }").unwrap();
    assert_eq!(format!("{}", val), "42");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_field_access_bool() {
    let (val, sch) = eval_expr("{ let p = @{ x: 42, y: true }; p.y }").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_field_access_nested() {
    let (val, sch) = eval_expr("{ let p = @{ inner: @{ x: 1 } }; p.inner.x }").unwrap();
    assert_eq!(format!("{}", val), "1");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
#[should_panic(expected = "UnknownField")]
fn test_field_access_missing_field() {
    let _ = eval_expr("{ let p = @{ x: 42 }; p.y }").unwrap();
}

#[test]
#[should_panic(expected = "ExpectedRecord(")]
fn test_field_access_non_record() {
    let _ = eval_expr("{ let n = 42; n.x }").unwrap();
}

use algo_j::api::eval_program;

#[test]
fn test_user_defined_record_variant_field_access() {
    let (val, sch) = eval_program(
        "type Point = P @{ x: Int, y: Int };
         let p = P @{ x: 10, y: 20 };
         match (p) {
             P @{ x, y } => x
         }"
    ).unwrap();
    assert_eq!(format!("{}", val), "10");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_user_defined_record_variant_nested_field_access() {
    let (val, sch) = eval_program(
        "type Wrapper = W @{ inner: @{ x: Int } };
         let w = W @{ inner: @{ x: 42 } };
         match (w) {
             W @{ inner } => inner.x
         }"
    ).unwrap();
    assert_eq!(format!("{}", val), "42");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_user_defined_record_variant_direct_field_access() {
    let (val, sch) = eval_program(
        "type Point = P @{ x: Int, y: Int };
         match (P @{ x: 3, y: 4 }) {
             P @{ x: a, y: b } => @{ sum: a + b }.sum
         }"
    ).unwrap();
    assert_eq!(format!("{}", val), "7");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}
