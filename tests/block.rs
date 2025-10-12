use algo_j::api::eval_expr;

#[test]
fn test_empty_block() {
    let (val, sch) = eval_expr("{}").unwrap();
    assert_eq!(format!("{}", val), "()");
    assert_eq!(format!("{}", sch.pretty()), "()");
}

#[test]
fn test_trailing_semicolon() {
    let (val, sch) = eval_expr("{ 1; 2; }").unwrap();
    assert_eq!(format!("{}", val), "()");
    assert_eq!(format!("{}", sch.pretty()), "()");
}
