use phox::api::eval;

#[test]
fn test_builtin_infix_eq() {
    let (val, sch) = eval("1 == 1").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");

    let (val, sch) = eval("1 == 2").unwrap();
    assert_eq!(format!("{}", val), "false");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_builtin_infix_arithmetic() {
    let (val, sch) = eval("3 + 4 * 2").unwrap();
    assert_eq!(format!("{}", val), "11");
    assert_eq!(format!("{}", sch.pretty()), "Int");

    let (val, sch) = eval("3 + 4 / 2").unwrap();
    assert_eq!(format!("{}", val), "5");
    assert_eq!(format!("{}", sch.pretty()), "Int");

    let (val, sch) = eval("10 - 3").unwrap();
    assert_eq!(format!("{}", val), "7");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_builtin_prefix_negate_and_not() {
    let (val, sch) = eval("negate 5").unwrap();
    assert_eq!(format!("{}", val), "-5");
    assert_eq!(format!("{}", sch.pretty()), "Int");

    let (val, sch) = eval("let x = 10; -x").unwrap();
    assert_eq!(format!("{}", val), "-10");
    assert_eq!(format!("{}", sch.pretty()), "Int");

    let (val, sch) = eval("! false").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");

    let (val, sch) = eval("not false").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_user_defined_infix_operator() {
    let (val, sch) = eval(
        "let (%%) = λx.λy. x * y + 1;
         3 %% 4"
    ).unwrap();
    assert_eq!(format!("{}", val), "13");
    assert_eq!(format!("{}", sch.pretty()), "Int");

    // prefix form
    let (val, sch) = eval(
        "let (%%) = λx.λy. x * y + 1;
         (%%) 2 5"
    ).unwrap();
    assert_eq!(format!("{}", val), "11");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_builtin_infix_comparisons() {
    let (val, sch) = eval("3 < 5").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");

    let (val, sch) = eval("3 <= 3").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");

    let (val, sch) = eval("4 > 2").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");

    let (val, sch) = eval("4 >= 5").unwrap();
    assert_eq!(format!("{}", val), "false");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_builtin_operators_as_functions() {
    // equality as function
    let (val, sch) = eval("(==) 2 2").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");

    // arithmetic as function
    let (val, sch) = eval("(+) 2 3").unwrap();
    assert_eq!(format!("{}", val), "5");
    assert_eq!(format!("{}", sch.pretty()), "Int");

    let (val, sch) = eval("(-) 7 4").unwrap();
    assert_eq!(format!("{}", val), "3");
    assert_eq!(format!("{}", sch.pretty()), "Int");

    let (val, sch) = eval("(*) 6 7").unwrap();
    assert_eq!(format!("{}", val), "42");
    assert_eq!(format!("{}", sch.pretty()), "Int");

    let (val, sch) = eval("(/) 9 2").unwrap();
    assert_eq!(format!("{}", val), "4"); // 整数除算
    assert_eq!(format!("{}", sch.pretty()), "Int");

    // comparison as function
    let (val, sch) = eval("(<) 1 2").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");

    let (val, sch) = eval("(>=) 3 3").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}
