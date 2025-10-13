use algo_j::api::eval_expr;

#[test]
fn test_none() {
    let (val, sch) = eval_expr("None").unwrap();
    assert_eq!(format!("{}", val), "None");
    assert_eq!(format!("{}", sch.pretty()), "∀ a. Option a");
}

#[test]
fn test_some_literal() {
    let (val, sch) = eval_expr("Some 10").unwrap();
    assert_eq!(format!("{}", val), "Some 10");
    assert_eq!(format!("{}", sch), "Option Int");
}

#[test]
fn test_cons_nil() {
    let (val, sch) = eval_expr("Cons 1 Nil").unwrap();
    assert_eq!(format!("{}", val), "Cons 1 Nil");
    assert_eq!(format!("{}", sch), "List Int");
}

#[test]
fn test_nested_cons() {
    let (val, sch) = eval_expr("Cons 1 (Cons 2 Nil)").unwrap();
    assert_eq!(format!("{}", val), "Cons 1 (Cons 2 Nil)");
    assert_eq!(format!("{}", sch), "List Int");
}

#[test]
fn test_addition() {
    let (val, sch) = eval_expr("2 + 3").unwrap();
    assert_eq!(format!("{}", val), "5");
    assert_eq!(format!("{}", sch), "Int");
}

#[test]
fn test_tuple() {
    let (val, sch) = eval_expr("(1, Some 2)").unwrap();
    assert_eq!(format!("{}", val), "(1, Some 2)");
    assert_eq!(format!("{}", sch), "(Int, Option Int)");
}

#[test]
fn test_singleton_tuple() {
    let (val, sch) = eval_expr("(42,)").unwrap();
    assert_eq!(format!("{}", val), "(42,)");
    assert_eq!(format!("{}", sch), "(Int,)"); // 1要素タプルの型表記
}

#[test]
fn test_let_simple() {
    // let x = 10 ; x + 1
    let (val, sch) = eval_expr("let x = 10 ; x + 1").unwrap();
    assert_eq!(format!("{}", val), "11");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_let_shadowing() {
    // let x = 5 ; let x = x + 1 ; x * 2
    let (val, sch) = eval_expr("let x = 5 ; let x = x + 1 ; x * 2").unwrap();
    assert_eq!(format!("{}", val), "12");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_let_tuple() {
    // let t = (1, Some 2) ; t
    let (val, sch) = eval_expr("let t = (1, Some 2) ; t").unwrap();
    assert_eq!(format!("{}", val), "(1, Some 2)");
    assert_eq!(format!("{}", sch.pretty()), "(Int, Option Int)");
}

#[test]
fn test_let_polymorphism_id() {
    // let id = \x. x ; (id 1, id true)
    let (val, sch) = eval_expr("let id = \\x. x ; (id 1, id true)").unwrap();
    assert_eq!(format!("{}", val), "(1, true)");
    assert_eq!(format!("{}", sch.pretty()), "(Int, Bool)");
}

#[test]
fn test_if_expression() {
    let (val, sch) = eval_expr("if (true) 1 else 2").unwrap();
    assert_eq!(format!("{}", val), "1");
    assert_eq!(format!("{}", sch.pretty()), "Int");

    let (val, sch) = eval_expr("if (false) 1 else 2").unwrap();
    assert_eq!(format!("{}", val), "2");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_cmp_in_if() {
    let (val, sch) = eval_expr("if (0 == 0) 42 else 99").unwrap();
    assert_eq!(format!("{}", val), "42");
    assert_eq!(format!("{}", sch.pretty()), "Int");

    let (val, sch) = eval_expr("if (1 == 0) 42 else 99").unwrap();
    assert_eq!(format!("{}", val), "99");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_cmp_eq() {
    let (val, sch) = eval_expr("1 == 1").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");

    let (val, sch) = eval_expr("1 == 2").unwrap();
    assert_eq!(format!("{}", val), "false");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_cmp_neq() {
    let (val, sch) = eval_expr("1 != 1").unwrap();
    assert_eq!(format!("{}", val), "false");
    assert_eq!(format!("{}", sch.pretty()), "Bool");

    let (val, sch) = eval_expr("1 != 2").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_cmp_lt() {
    let (val, sch) = eval_expr("1 < 1").unwrap();
    assert_eq!(format!("{}", val), "false");
    assert_eq!(format!("{}", sch.pretty()), "Bool");

    let (val, sch) = eval_expr("1 < 2").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_cmp_le() {
    let (val, sch) = eval_expr("1 <= 0").unwrap();
    assert_eq!(format!("{}", val), "false");
    assert_eq!(format!("{}", sch.pretty()), "Bool");

    let (val, sch) = eval_expr("1 <= 1").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");

    let (val, sch) = eval_expr("1 <= 2").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_cmp_gt() {
    let (val, sch) = eval_expr("1 > 1").unwrap();
    assert_eq!(format!("{}", val), "false");
    assert_eq!(format!("{}", sch.pretty()), "Bool");

    let (val, sch) = eval_expr("2 > 1").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_cmp_ge() {
    let (val, sch) = eval_expr("0 >= 1").unwrap();
    assert_eq!(format!("{}", val), "false");
    assert_eq!(format!("{}", sch.pretty()), "Bool");

    let (val, sch) = eval_expr("1 >= 1").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");

    let (val, sch) = eval_expr("2 >= 1").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_if_with_eq_and_add() {
    let (val, sch) = eval_expr("if (1 + 2 == 3) 42 else 0").unwrap();
    assert_eq!(format!("{}", val), "42");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_cmp_mixed_right() {
    let (val, sch) = eval_expr("if (1 < 2 + 3) 1 else 0").unwrap();
    assert_eq!(format!("{}", val), "1");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_op_priority() {
    let (val, sch) = eval_expr("1 + 2 * 3 == 7").unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_letrec_factorial() {
    // let rec fact = \n. if (n == 0) 1 else n * fact (n - 1) ; fact 5
    let (val, sch) = eval_expr(
        "let rec fact = \\n. if (n == 0) 1 else n * fact (n - 1) ; fact 5"
    ).unwrap();
    assert_eq!(format!("{}", val), "120");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_letrec_fibonacci() {
    // let rec fib = \n. if (n == 0) 0 else if (n == 1) 1 else fib (n - 1) + fib (n - 2) ; fib 6
    let (val, sch) = eval_expr(
        "let rec fib = \\n. if (n == 0) 0 else if (n == 1) 1 else fib (n - 1) + fib (n - 2) ; fib 6"
    ).unwrap();
    assert_eq!(format!("{}", val), "8");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_letrec_immediate_call() {
    // 定義直後に呼ぶパターン
    let (val, sch) = eval_expr(
        "let rec id = \\x. x ; id 42"
    ).unwrap();
    assert_eq!(format!("{}", val), "42");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_letrec_mutual_simple() {
    // まだ相互再帰未対応ならスキップ or 失敗を確認
    // まずは単一変数の let rec のみ対応にしておく
}

#[test]
fn test_fib_conditions() {
    let (val, _) = eval_expr("let rec fib = \\n. if (n == 0) 0 else if (n == 1) 1 else 999 ; fib 0").unwrap();
    assert_eq!(format!("{}", val), "0");

    let (val, _) = eval_expr("let rec fib = \\n. if (n == 0) 0 else if (n == 1) 1 else 999 ; fib 1").unwrap();
    assert_eq!(format!("{}", val), "1");

    let (val, _) = eval_expr("let rec fib = \\n. if (n == 0) 0 else if (n == 1) 1 else 999 ; fib 2").unwrap();
    assert_eq!(format!("{}", val), "999");
}

// --------------
#[test]
fn test_pat_var() {
    let (val, sch) = eval_expr("let x = 42 ; x").unwrap();
    assert_eq!(format!("{}", val), "42");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_pat_wildcard() {
    let (val, sch) = eval_expr("let _ = 42 ; 0").unwrap();
    assert_eq!(format!("{}", val), "0");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_pat_lit() {
    let (val, sch) = eval_expr("let 1 = 1 ; 99").unwrap();
    assert_eq!(format!("{}", val), "99");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

// --------------
#[test]
fn test_pat_tuple_simple() {
    let (val, sch) = eval_expr("let (x, y) = (1, 2) ; x + y").unwrap();
    assert_eq!(format!("{}", val), "3");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_pat_tuple_nested() {
    let (val, sch) = eval_expr("let (x, (y, z)) = (1, (2, 3)) ; x + y + z").unwrap();
    assert_eq!(format!("{}", val), "6");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

// --------------
#[test]
fn test_pat_con_list() {
    let (val, sch) = eval_expr("let Cons x xs = Cons 1 (Cons 2 Nil) ; x").unwrap();
    assert_eq!(format!("{}", val), "1");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_pat_con_list_nested() {
    let (val, sch) = eval_expr("let Cons x (Cons y ys) = Cons 1 (Cons 2 Nil) ; x + y").unwrap();
    assert_eq!(format!("{}", val), "3");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

// --------------
#[test]
#[should_panic]
fn test_pat_tuple_mismatch() {
    let _ = eval_expr("let (x, y) = (1,) ; x").unwrap();
}

#[test]
#[should_panic]
fn test_pat_con_mismatch() {
    let _ = eval_expr("let Cons x xs = Nil ; x").unwrap();
}
