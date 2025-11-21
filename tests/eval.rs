use phox::api::*;

#[test]
fn test_none() {
    let (val, sch) = eval("None").unwrap();
    assert_eq!(format!("{}", val), "None");
    assert_eq!(format!("{}", sch.pretty()), "∀ a. Option a");
}

#[test]
fn test_some_literal() {
    let (val, sch) = eval("Some 10").unwrap();
    assert_eq!(format!("{}", val), "Some 10");
    assert_eq!(format!("{}", sch), "Option Int");
}

#[test]
fn test_cons_nil() {
    let (val, sch) = eval("Cons 1 Nil").unwrap();
    assert_eq!(format!("{}", val), "Cons 1 Nil");
    assert_eq!(format!("{}", sch), "List Int");
}

#[test]
fn test_nested_cons() {
    let (val, sch) = eval("Cons 1 (Cons 2 Nil)").unwrap();
    assert_eq!(format!("{}", val), "Cons 1 (Cons 2 Nil)");
    assert_eq!(format!("{}", sch), "List Int");
}

#[test]
fn test_addition() {
    let (val, sch) = eval("2 + 3").unwrap();
    assert_eq!(format!("{}", val), "5");
    assert_eq!(format!("{}", sch), "Int");
}

#[test]
fn test_tuple() {
    let (val, sch) = eval("(1, Some 2)").unwrap();
    assert_eq!(format!("{}", val), "(1, Some 2)");
    assert_eq!(format!("{}", sch), "(Int, Option Int)");
}

#[test]
fn test_singleton_tuple() {
    let (val, sch) = eval("(42,)").unwrap();
    assert_eq!(format!("{}", val), "(42,)");
    assert_eq!(format!("{}", sch), "(Int,)"); // 1要素タプルの型表記
}

#[test]
fn test_if_expression() {
    let (val, sch) = eval("if (true) 1 else 2").unwrap();
    assert_eq!(format!("{}", val), "1");
    assert_eq!(format!("{}", sch.pretty()), "Int");

    let (val, sch) = eval("if (false) 1 else 2").unwrap();
    assert_eq!(format!("{}", val), "2");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_cmp_in_if() {
    let (val, sch) = eval("if (0 == 0) 42 else 99").unwrap();
    assert_eq!(format!("{}", val), "42");
    assert_eq!(format!("{}", sch.pretty()), "Int");

    let (val, sch) = eval("if (1 == 0) 42 else 99").unwrap();
    assert_eq!(format!("{}", val), "99");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_cmp_eq() {
    let (val, sch) = eval("1 == 1").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");

    let (val, sch) = eval("1 == 2").unwrap();
    assert_eq!(format!("{}", val), "false");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_cmp_neq() {
    let (val, sch) = eval("1 != 1").unwrap();
    assert_eq!(format!("{}", val), "false");
    assert_eq!(format!("{}", sch.pretty()), "Bool");

    let (val, sch) = eval("1 != 2").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_cmp_lt() {
    let (val, sch) = eval("1 < 1").unwrap();
    assert_eq!(format!("{}", val), "false");
    assert_eq!(format!("{}", sch.pretty()), "Bool");

    let (val, sch) = eval("1 < 2").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_cmp_le() {
    let (val, sch) = eval("1 <= 0").unwrap();
    assert_eq!(format!("{}", val), "false");
    assert_eq!(format!("{}", sch.pretty()), "Bool");

    let (val, sch) = eval("1 <= 1").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");

    let (val, sch) = eval("1 <= 2").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_cmp_gt() {
    let (val, sch) = eval("1 > 1").unwrap();
    assert_eq!(format!("{}", val), "false");
    assert_eq!(format!("{}", sch.pretty()), "Bool");

    let (val, sch) = eval("2 > 1").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_cmp_ge() {
    let (val, sch) = eval("0 >= 1").unwrap();
    assert_eq!(format!("{}", val), "false");
    assert_eq!(format!("{}", sch.pretty()), "Bool");

    let (val, sch) = eval("1 >= 1").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");

    let (val, sch) = eval("2 >= 1").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_if_with_eq_and_add() {
    let (val, sch) = eval("if (1 + 2 == 3) 42 else 0").unwrap();
    assert_eq!(format!("{}", val), "42");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_cmp_mixed_right() {
    let (val, sch) = eval("if (1 < 2 + 3) 1 else 0").unwrap();
    assert_eq!(format!("{}", val), "1");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_op_priority() {
    let (val, sch) = eval("1 + 2 * 3 == 7").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}
