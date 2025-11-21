use phox::api::*;

#[test]
fn unary_neg_literal() {
    let (val, sch) = eval("-5").unwrap();
    assert_eq!(format!("{}", val), "-5");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn unary_neg_variable() {
    let (val, sch) = eval("let x = 3; -x").unwrap();
    assert_eq!(format!("{}", val), "-3");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn unary_neg_function_application() {
    let (val, sch) = eval("let f = \\y. y + 1; - f 4").unwrap();
    assert_eq!(format!("{}", val), "-5");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn abs_with_negative_literal() {
    let (val, sch) = eval(
        "let abs = \\x. if (0 > x) -x else x; (abs -5, abs 7)"
    ).unwrap();
    assert_eq!(format!("{}", val), "(5, 7)");
    assert_eq!(format!("{}", sch.pretty()), "(Int, Int)");
}

#[test]
fn not_true_false() {
    let (val, sch) = eval("not true").unwrap();
    assert_eq!(format!("{}", val), "false");
    assert_eq!(format!("{}", sch.pretty()), "Bool");

    let (val, sch) = eval("!false").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn not_comparison() {
    let (val, sch) = eval("not (3 == 4)").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn mixed_unary_ops_with_abs() {
    let (val, sch) = eval(r#"
        let abs = \x. if (0 > x) -x else x;
        let x = -10;
        let y = not (x < 0);
        (y, abs x)
    "#).unwrap();

    assert_eq!(format!("{}", val), "(false, 10)");
    assert_eq!(format!("{}", sch.pretty()), "(Bool, Int)");
}
