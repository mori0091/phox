use phox::api::eval_program;

#[test]
fn test_trait_eq_int() {
    let src = r#"
        trait Eq a {
            eq  : a -> a -> Bool;
            neq : a -> a -> Bool;
        };
        impl Eq Int {
            eq  = \a.\b. a == b;
            neq = \a.\b. a != b;
        };
        eq 1 1
    "#;
    let (val, sch) = eval_program(src).unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_trait_record_access() {
    let src = r#"
        trait Eq a {
            eq  : a -> a -> Bool;
            neq : a -> a -> Bool;
        };
        impl Eq Int {
            eq  = \a.\b. a == b;
            neq = \a.\b. a != b;
        };
        let d = @{Eq Int};
        d.eq 1 2
    "#;
    let (val, sch) = eval_program(src).unwrap();
    assert_eq!(format!("{}", val), "false");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_trait_record_infix() {
    let src = r#"
        trait Eq a {
            eq  : a -> a -> Bool;
            neq : a -> a -> Bool;
        };
        impl Eq Int {
            eq  = \a.\b. a == b;
            neq = \a.\b. a != b;
        };
        let d = @{Eq Int};
        1 `d.eq` 1
    "#;
    let (val, sch) = eval_program(src).unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_trait_record_first_class() {
    let src = r#"
        trait Eq a {
            eq  : a -> a -> Bool;
            neq : a -> a -> Bool;
        };
        impl Eq Int {
            eq  = \a.\b. a == b;
            neq = \a.\b. a != b;
        };
        // let f = \@{eq:e, neq:_}.\x.\y. e x y;
        let f = \@{eq, neq}.\x.\y. eq x y;  // same as the above
        f @{Eq Int} 3 4
    "#;
    let (val, sch) = eval_program(src).unwrap();
    assert_eq!(format!("{}", val), "false");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

// ------------------------

#[test]
#[should_panic(expected = "unbound variable: eq:")]
fn test_unimplemented_trait_error() {
    // Bool に対して Eq が未実装
    let src = r#"
        trait Eq a { eq : a -> a -> Bool; };
        eq true false
    "#;
    eval_program(src).unwrap();
    // let err = eval_program(src).unwrap_err();
    // assert!(format!("{}", err).contains("no implementation for Eq Bool"));
}

#[test]
fn test_unbound_trait_record_error() {
    // @{Eq Bool} を作ろうとするが未実装
    let src = r#"
        trait Eq a { eq : a -> a -> Bool; };
        @{Eq Bool}
    "#;
    let err = eval_program(src).unwrap_err();
    // assert!(format!("{}", err).contains("no implementation for Eq Bool"));
    assert!(format!("{}", err).contains("MissingTraitImpl"));
}

#[test]
fn test_field_access_on_var_error() {
    // row polymorphism がないので dict.eq は失敗する
    let src = r#"
        trait Eq a { eq : a -> a -> Bool; neq : a -> a -> Bool; };
        impl Eq Int { eq = \a.\b. a == b; neq = \a.\b. a != b; };
        let f = \dict.\x.\y. dict.eq x y;
        f @{Eq Int} 1 2
    "#;
    let err = eval_program(src).unwrap_err();
    assert!(format!("{}", err).contains("ExpectedRecord"));
}

#[test]
fn test_trait_member_conflict_ok() {
    // 同じ名前のメンバを持つ trait を同じ型に実装
    let src = r#"
        trait Foo a { f : a -> a; };
        trait Bar a { f : a -> a; };
        impl Foo Int { f = \x. x; };
        impl Bar Int { f = \x. x; };
    "#;
    eval_program(src).unwrap();
}

#[test]
fn test_trait_member_conflict_error() {
    // 同じ名前のメンバを持つ trait を同じ型に実装 → 衝突
    let src = r#"
        trait Foo a { f : a -> a; };
        trait Bar a { f : a -> a; };
        impl Foo Int { f = \x. x; };
        impl Bar Int { f = \x. x; };
        f 100
    "#;
    let err = eval_program(src).unwrap_err();
    assert!(format!("{}", err).contains("AmbiguousVariable"));
}

#[test]
fn test_trait_member_non_conflict_1() {
    // 同じ名前のメンバを持つ trait を同じ型に実装(型違い)
    let src = r#"
        trait Foo a { f : a -> Bool; };
        trait Bar a { f : a -> a; };
        impl Foo Int { f = \x. 0 <= x; };
        impl Bar Int { f = \x. x; };
        f 100
    "#;
    let err = eval_program(src).unwrap_err();
    assert!(format!("{}", err).contains("AmbiguousVariable"));
}

#[test]
fn test_trait_member_non_conflict_2() {
    // 同じ名前のメンバを持つ trait を同じ型に実装(型違い)
    let src = r#"
        trait Foo a { f : a -> Bool; };
        trait Bar a { f : a -> a; };
        impl Foo Int { f = \x. 0 <= x; };
        impl Bar Int { f = \x. x; };
        @{Bar Int}.f 100
    "#;
    let (val, sch) = eval_program(src).unwrap();
    assert_eq!(format!("{}", val), "100");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_trait_member_non_conflict_3() {
    // 同じ名前のメンバを持つ trait を同じ型に実装(型違い)
    let src = r#"
        trait Foo a { f : a -> Bool; };
        trait Bar a { f : a -> a; };
        impl Foo Int { f = \x. 0 <= x; };
        impl Bar Int { f = \x. x; };
        @{Foo Int}.f 100
    "#;
    let (val, sch) = eval_program(src).unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_trait_member_non_conflict_4() {
    // 同じ名前のメンバを持つ trait を同じ型に実装(型違い)
    let src = r#"
        trait Foo a { f : Bool -> a; };
        trait Bar a { f : a -> a; };
        impl Foo Int { f = \x. if (x) 1 else 0; };
        impl Bar Int { f = \x. x; };
        f true
    "#;
    let err = eval_program(src).unwrap_err();
    assert!(format!("{}", err).contains("AmbiguousVariable"));
}

#[test]
fn test_trait_member_non_conflict_5() {
    // 同じ名前のメンバを持つ trait を同じ型に実装(型違い)
    let src = r#"
        trait Foo a { f : Bool -> a; };
        trait Bar a { f : a -> a; };
        impl Foo Int { f = \x. if (x) 1 else 0; };
        impl Bar Int { f = \x. x; };
        f 100
    "#;
    let err = eval_program(src).unwrap_err();
    assert!(format!("{}", err).contains("AmbiguousVariable"));
}
