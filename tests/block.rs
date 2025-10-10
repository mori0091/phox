use algo_j::api::eval_test;

#[test]
fn test_empty_block() {
    let (val, sch) = eval_test("{}").unwrap();
    assert_eq!(format!("{}", val), "()");
    assert_eq!(format!("{}", sch.pretty()), "()");
}

#[test]
fn test_trailing_semicolon() {
    let (val, sch) = eval_test("{ 1; 2; }").unwrap();
    assert_eq!(format!("{}", val), "()");
    assert_eq!(format!("{}", sch.pretty()), "()");
}
