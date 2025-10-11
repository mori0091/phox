use algo_j::api::eval_test;

#[test]
fn test_match_bool() {
    let (val, sch) = eval_test("match(true) { true => 1 }").unwrap();
    assert_eq!(format!("{}", val), "1");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_match_tuple() {
    let (val, sch) = eval_test("match((1, 2)) { (x, y) => x + y }").unwrap();
    assert_eq!(format!("{}", val), "3");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_match_option_some() {
    let (val, sch) = eval_test("match(Some 1) { Some x => x, None => 0 }").unwrap();
    assert_eq!(format!("{}", val), "1");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_struct_value() {
    let (val, sch) = eval_test("Point@{ x: 1, y: 2 }").unwrap();
    assert_eq!(format!("{}", val), "Point@{ x: 1, y: 2 }");
    assert_eq!(format!("{}", sch.pretty()), "Point@{ x: Int, y: Int }");
}

#[test]
fn test_struct_match() {
    let (val, sch) = eval_test(
        "match(Point@{ x: 1, y: 2 }) { Point@{ x: x, y: y } => x + y }"
    ).unwrap();
    assert_eq!(format!("{}", val), "3");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_struct_match_ignore_field() {
    let (val, sch) = eval_test(
        "match(Point@{ x: 10, y: 20 }) { Point@{ x: x, y: _ } => x }"
    ).unwrap();
    assert_eq!(format!("{}", val), "10");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}
