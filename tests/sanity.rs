use algo_j::api::eval_expr;

#[test]
#[should_panic]
fn sanity_match_empty_arm_no_comma() {
    let _ = eval_expr("match(1) { }").unwrap();
}

#[test]
#[should_panic]
fn sanity_match_empty_arm_with_comma() {
    let _ = eval_expr("match(1) { , }").unwrap();
}

#[test]
fn sanity_match_single_arm_no_comma() {
    let (val, sch) = eval_expr("match(1) { x => x }").unwrap();
    assert_eq!(format!("{}", val), "1");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn sanity_match_single_arm_with_comma() {
    let (val, sch) = eval_expr("match(1) { x => x, }").unwrap();
    assert_eq!(format!("{}", val), "1");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn sanity_match_multiple_arms_no_comma() {
    let (val, sch) = eval_expr("match(true) { true => 1, false => 2 }").unwrap();
    assert_eq!(format!("{}", val), "1");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn sanity_match_multiple_arms_with_comma() {
    let (val, sch) = eval_expr("match(true) { true => 1, false => 2, }").unwrap();
    assert_eq!(format!("{}", val), "1");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn sanity_record_empty_fields_no_comma() {
    let (val, sch) = eval_expr("@{}").unwrap();
    assert_eq!(format!("{}", val), "@{}");
    assert_eq!(format!("{}", sch.pretty()), "@{}");
}

#[test]
#[should_panic]
fn sanity_record_empty_fields_with_comma() {
    let _ = eval_expr("@{,}").unwrap();
}

#[test]
fn sanity_record_single_field_no_comma() {
    let (val, sch) = eval_expr("@{ a: 1 }").unwrap();
    assert_eq!(format!("{}", val), "@{ a: 1 }");
    assert_eq!(format!("{}", sch.pretty()), "@{ a: Int }");
}

#[test]
fn sanity_record_single_field_with_comma() {
    let (val, sch) = eval_expr("@{ a: 1, }").unwrap();
    assert_eq!(format!("{}", val), "@{ a: 1 }");
    assert_eq!(format!("{}", sch.pretty()), "@{ a: Int }");
}

#[test]
fn sanity_record_multiple_fields_no_comma() {
    let (val, sch) = eval_expr("@{ x: 1, y: 2 }").unwrap();
    assert_eq!(format!("{}", val), "@{ x: 1, y: 2 }");
    assert_eq!(format!("{}", sch.pretty()), "@{ x: Int, y: Int }");
}

#[test]
fn sanity_record_multiple_fields_with_comma() {
    let (val, sch) = eval_expr("@{ x: 1, y: 2, }").unwrap();
    assert_eq!(format!("{}", val), "@{ x: 1, y: 2 }");
    assert_eq!(format!("{}", sch.pretty()), "@{ x: Int, y: Int }");
}

#[test]
fn sanity_record_pattern_field_order_irrelevant() {
    let (val, sch) = eval_expr(
        "match(@{ y: 2, x: 1 }) { @{ x: x, y: y } => x + y }"
    ).unwrap();
    assert_eq!(format!("{}", val), "3");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
#[should_panic]
fn sanity_record_pattern_too_less_field() {
    let _ = eval_expr(
        "match(@{ y: 2, x: 1 }) { @{ x: x } => x }"
    ).unwrap();
}

#[test]
#[should_panic]
fn sanity_record_pattern_too_much_field() {
    let _ = eval_expr(
        "match(@{ y: 2, x: 1 }) { @{ x: x, y: y, z: _ } => x + y }"
    ).unwrap();
}
