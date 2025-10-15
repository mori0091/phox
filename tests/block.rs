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

// --------------

#[test]
fn test_block_with_expr_tail() {
    // { let x = 1; let y = 2; x + y }
    let (val, sch) = eval_expr("{ let x = 1; let y = 2; x + y }").unwrap();
    assert_eq!(format!("{}", val), "3");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_block_with_only_stmts() {
    // { let x = 42; let y = x; }
    let (val, sch) = eval_expr("{ let x = 42; let y = x; }").unwrap();
    assert_eq!(format!("{}", val), "()");
    assert_eq!(format!("{}", sch.pretty()), "()");
}

#[test]
fn test_nested_blocks() {
    // { let x = { let y = 1; y + 2 }; x * 3 }
    let (val, sch) = eval_expr("{ let x = { let y = 1; y + 2 }; x * 3 }").unwrap();
    assert_eq!(format!("{}", val), "9");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}
