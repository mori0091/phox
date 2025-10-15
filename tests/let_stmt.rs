use algo_j::api::eval_program;

#[test]
fn test_let_simple() {
    // let x = 10 ; x + 1
    let (val, sch) = eval_program("let x = 10 ; x + 1").unwrap();
    assert_eq!(format!("{}", val), "11");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_let_shadowing() {
    // let x = 5 ; let x = x + 1 ; x * 2
    let (val, sch) = eval_program("let x = 5 ; let x = x + 1 ; x * 2").unwrap();
    assert_eq!(format!("{}", val), "12");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_let_tuple() {
    // let t = (1, Some 2) ; t
    let (val, sch) = eval_program("let t = (1, Some 2) ; t").unwrap();
    assert_eq!(format!("{}", val), "(1, Some 2)");
    assert_eq!(format!("{}", sch.pretty()), "(Int, Option Int)");
}

#[test]
fn test_let_polymorphism_id() {
    // let id = \x. x ; (id 1, id true)
    let (val, sch) = eval_program("let id = \\x. x ; (id 1, id true)").unwrap();
    assert_eq!(format!("{}", val), "(1, true)");
    assert_eq!(format!("{}", sch.pretty()), "(Int, Bool)");
}

// --------------
#[test]
fn test_letrec_factorial() {
    // let rec fact = \n. if (n == 0) 1 else n * fact (n - 1) ; fact 5
    let (val, sch) = eval_program(
        "let rec fact = \\n. if (n == 0) 1 else n * fact (n - 1) ; fact 5"
    ).unwrap();
    assert_eq!(format!("{}", val), "120");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_letrec_fibonacci() {
    // let rec fib = \n. if (n == 0) 0 else if (n == 1) 1 else fib (n - 1) + fib (n - 2) ; fib 6
    let (val, sch) = eval_program(
        "let rec fib = \\n. if (n == 0) 0 else if (n == 1) 1 else fib (n - 1) + fib (n - 2) ; fib 6"
    ).unwrap();
    assert_eq!(format!("{}", val), "8");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_letrec_immediate_call() {
    // 定義直後に呼ぶパターン
    let (val, sch) = eval_program(
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
    let (val, _) = eval_program("let rec fib = \\n. if (n == 0) 0 else if (n == 1) 1 else 999 ; fib 0").unwrap();
    assert_eq!(format!("{}", val), "0");

    let (val, _) = eval_program("let rec fib = \\n. if (n == 0) 0 else if (n == 1) 1 else 999 ; fib 1").unwrap();
    assert_eq!(format!("{}", val), "1");

    let (val, _) = eval_program("let rec fib = \\n. if (n == 0) 0 else if (n == 1) 1 else 999 ; fib 2").unwrap();
    assert_eq!(format!("{}", val), "999");
}

// --------------
#[test]
fn test_pat_var() {
    let (val, sch) = eval_program("let x = 42 ; x").unwrap();
    assert_eq!(format!("{}", val), "42");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_pat_wildcard() {
    let (val, sch) = eval_program("let _ = 42 ; 0").unwrap();
    assert_eq!(format!("{}", val), "0");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_pat_lit() {
    let (val, sch) = eval_program("let 1 = 1 ; 99").unwrap();
    assert_eq!(format!("{}", val), "99");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

// --------------
#[test]
fn test_pat_tuple_simple() {
    let (val, sch) = eval_program("let (x, y) = (1, 2) ; x + y").unwrap();
    assert_eq!(format!("{}", val), "3");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_pat_tuple_nested() {
    let (val, sch) = eval_program("let (x, (y, z)) = (1, (2, 3)) ; x + y + z").unwrap();
    assert_eq!(format!("{}", val), "6");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

// --------------
#[test]
fn test_pat_con_list() {
    let (val, sch) = eval_program("let Cons x xs = Cons 1 (Cons 2 Nil) ; x").unwrap();
    assert_eq!(format!("{}", val), "1");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_pat_con_list_nested() {
    let (val, sch) = eval_program("let Cons x (Cons y ys) = Cons 1 (Cons 2 Nil) ; x + y").unwrap();
    assert_eq!(format!("{}", val), "3");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

// --------------
#[test]
#[should_panic]
fn test_pat_tuple_mismatch() {
    let _ = eval_program("let (x, y) = (1,) ; x").unwrap();
}

#[test]
#[should_panic]
fn test_pat_con_mismatch() {
    let _ = eval_program("let Cons x xs = Nil ; x").unwrap();
}

// --------------
#[test]
fn test_let_and_use() {
    // let x = 5 ; let y = x * 2 ; y + 3
    let (val, sch) = eval_program("let x = 5 ; let y = x * 2 ; y + 3").unwrap();
    assert_eq!(format!("{}", val), "13");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_nested_block_top_level() {
    // let x = { let y = 2 ; y + 1 } ; x * 10
    let (val, sch) = eval_program("let x = { let y = 2 ; y + 1 } ; x * 10").unwrap();
    assert_eq!(format!("{}", val), "30");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}
