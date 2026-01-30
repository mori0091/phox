use phox::api::*;

#[test]
fn test_match_bool() {
    let (val, sch) = eval("match(true) { true => 1 }").unwrap();
    assert_eq!(format!("{}", val), "1");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_match_tuple() {
    let (val, sch) = eval("match((1, 2)) { (x, y) => x + y }").unwrap();
    assert_eq!(format!("{}", val), "3");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_match_option_some() {
    let (val, sch) = eval("match(Some 1) { Some x => x, None => 0 }").unwrap();
    assert_eq!(format!("{}", val), "1");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_record_value() {
    let (val, sch) = eval("@{ x = 1, y = 2 }").unwrap();
    assert_eq!(format!("{}", val), "@{ x = 1, y = 2 }");
    assert_eq!(format!("{}", sch.pretty()), "@{ x: Int, y: Int }");
}

#[test]
fn test_record_match() {
    let (val, sch) = eval(
        "match(@{ x = 1, y = 2 }) { @{ x = x, y = y } => x + y }"
    ).unwrap();
    assert_eq!(format!("{}", val), "3");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_record_match_ignore_field() {
    let (val, sch) = eval(
        "match(@{ x = 10, y = 20 }) { @{ x = x, y = _ } => x }"
    ).unwrap();
    assert_eq!(format!("{}", val), "10");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}
