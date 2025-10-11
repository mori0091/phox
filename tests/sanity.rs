use algo_j::api::eval_test;

#[test]
#[should_panic]
fn sanity_match_empty_arm_no_comma() {
    let _ = eval_test("match(1) { }").unwrap();
}

#[test]
#[should_panic]
fn sanity_match_empty_arm_with_comma() {
    let _ = eval_test("match(1) { , }").unwrap();
}

#[test]
fn sanity_match_single_arm_no_comma() {
    let (val, sch) = eval_test("match(1) { x => x }").unwrap();
    assert_eq!(format!("{}", val), "1");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn sanity_match_single_arm_with_comma() {
    let (val, sch) = eval_test("match(1) { x => x, }").unwrap();
    assert_eq!(format!("{}", val), "1");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn sanity_match_multiple_arms_no_comma() {
    let (val, sch) = eval_test("match(true) { true => 1, false => 2 }").unwrap();
    assert_eq!(format!("{}", val), "1");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn sanity_match_multiple_arms_with_comma() {
    let (val, sch) = eval_test("match(true) { true => 1, false => 2, }").unwrap();
    assert_eq!(format!("{}", val), "1");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn sanity_struct_empty_fields_no_comma() {
    let (val, sch) = eval_test("struct Empty {}").unwrap();
    assert_eq!(format!("{}", val), "struct Empty {}");
    assert_eq!(format!("{}", sch.pretty()), "struct Empty {}");
}

#[test]
#[should_panic]
fn sanity_struct_empty_fields_with_comma() {
    let _ = eval_test("struct Empty {,}").unwrap();
}

#[test]
fn sanity_struct_single_field_no_comma() {
    let (val, sch) = eval_test("struct S { a: 1 }").unwrap();
    assert_eq!(format!("{}", val), "struct S { a: 1 }");
    assert_eq!(format!("{}", sch.pretty()), "struct S { a: Int }");
}

#[test]
fn sanity_struct_single_field_with_comma() {
    let (val, sch) = eval_test("struct S { a: 1, }").unwrap();
    assert_eq!(format!("{}", val), "struct S { a: 1 }");
    assert_eq!(format!("{}", sch.pretty()), "struct S { a: Int }");
}

#[test]
fn sanity_struct_multiple_fields_no_comma() {
    let (val, sch) = eval_test("struct P { x: 1, y: 2 }").unwrap();
    assert_eq!(format!("{}", val), "struct P { x: 1, y: 2 }");
    assert_eq!(format!("{}", sch.pretty()), "struct P { x: Int, y: Int }");
}

#[test]
fn sanity_struct_multiple_fields_with_comma() {
    let (val, sch) = eval_test("struct P { x: 1, y: 2, }").unwrap();
    assert_eq!(format!("{}", val), "struct P { x: 1, y: 2 }");
    assert_eq!(format!("{}", sch.pretty()), "struct P { x: Int, y: Int }");
}

#[test]
fn sanity_struct_pattern_field_order_irrelevant() {
    let (val, sch) = eval_test(
        "match(struct P { y: 2, x: 1 }) { struct P { x: x, y: y } => x + y }"
    ).unwrap();
    assert_eq!(format!("{}", val), "3");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
#[should_panic]
fn sanity_struct_pattern_too_less_field() {
    let _ = eval_test(
        "match(struct P { y: 2, x: 1 }) { struct P { x: x } => x }"
    ).unwrap();
}

#[test]
#[should_panic]
fn sanity_struct_pattern_too_much_field() {
    let _ = eval_test(
        "match(struct P { y: 2, x: 1 }) { struct P { x: x, y: y, z: _ } => x + y }"
    ).unwrap();
}
