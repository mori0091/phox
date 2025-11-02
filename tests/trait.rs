use phox::api::eval;

#[test]
fn test_trait_eq_int() {
    let src = r#"
        trait Eq2 a {
            eq  : a -> a -> Bool;
            neq : a -> a -> Bool;
        };
        impl Eq2 Int {
            eq  = \a.\b. @{Eq Int}.(==) a b;
            neq = \a.\b. @{Eq Int}.(!=) a b;
        };
        eq 1 1
    "#;
    let (val, sch) = eval(src).unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_trait_record_access() {
    let src = r#"
        trait Eq2 a {
            eq  : a -> a -> Bool;
            neq : a -> a -> Bool;
        };
        impl Eq2 Int {
            eq  = \a.\b. @{Eq Int}.(==) a b;
            neq = \a.\b. @{Eq Int}.(!=) a b;
        };
        let d = @{Eq2 Int};
        d.eq 1 2
    "#;
    let (val, sch) = eval(src).unwrap();
    assert_eq!(format!("{}", val), "false");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_trait_record_infix() {
    let src = r#"
        trait Eq2 a {
            eq  : a -> a -> Bool;
            neq : a -> a -> Bool;
        };
        impl Eq2 Int {
            eq  = \a.\b. @{Eq Int}.(==) a b;
            neq = \a.\b. @{Eq Int}.(!=) a b;
        };
        let d = @{Eq2 Int};
        1 `d.eq` 1
    "#;
    let (val, sch) = eval(src).unwrap();
    assert_eq!(format!("{}", val), "true");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_trait_record_first_class() {
    let src = r#"
        trait Eq2 a {
            eq  : a -> a -> Bool;
            neq : a -> a -> Bool;
        };
        impl Eq2 Int {
            eq  = \a.\b. @{Eq Int}.(==) a b;
            neq = \a.\b. @{Eq Int}.(!=) a b;
        };
        // let f = \@{eq:e, neq:_}.\x.\y. e x y;
        let f = \@{eq, neq}.\x.\y. eq x y;  // same as the above
        f @{Eq2 Int} 3 4
    "#;
    let (val, sch) = eval(src).unwrap();
    assert_eq!(format!("{}", val), "false");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

#[test]
fn test_trait_polymorphism() {
    let src = r#"
        trait Eq2 a {
            eq  : a -> a -> Bool;
            neq : a -> a -> Bool;
        };
        impl Eq2 Int {
            eq  = \a.\b. @{Eq Int}.(==) a b;
            neq = \a.\b. @{Eq Int}.(!=) a b;
        };
        impl Eq2 Bool {
            eq  = \a.\b. @{Eq Bool}.(==) a b;
            neq = \a.\b. @{Eq Bool}.(!=) a b;
        };
        eq 1 2
    "#;
    let (val, sch) = eval(src).unwrap();
    assert_eq!(format!("{}", val), "false");
    assert_eq!(format!("{}", sch.pretty()), "Bool");
}

// ------------------------

#[test]
fn test_unimplemented_trait_error() {
    // Bool に対して Eq が未実装
    let src = r#"
        trait Eq2 a { eq : a -> a -> Bool; };
        eq true false
    "#;
    let err = eval(src).unwrap_err();
    assert!(format!("{}", err).contains("infer error: UnboundVariable"));
}

#[test]
fn test_unbound_trait_record_error() {
    // @{Eq Bool} を作ろうとするが未実装
    let src = r#"
        trait Eq2 a { eq : a -> a -> Bool; };
        @{Eq2 Bool}
    "#;
    let err = eval(src).unwrap_err();
    assert!(format!("{}", err).contains("resolve error: no implementation for Eq2 Bool"));
}

#[test]
fn test_field_access_on_var_error() {
    // row polymorphism がないので dict.eq は失敗する
    let src = r#"
        trait Eq2 a {
            eq  : a -> a -> Bool;
            neq : a -> a -> Bool;
        };
        impl Eq2 Int {
            eq  = \a.\b. @{Eq Int}.(==) a b;
            neq = \a.\b. @{Eq Int}.(!=) a b;
        };
        let f = \dict.\x.\y. dict.eq x y;
        f @{Eq2 Int} 1 2
    "#;
    let err = eval(src).unwrap_err();
    assert!(format!("{}", err).contains("infer error: ExpectedRecord"));
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
    eval(src).unwrap();
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
    let err = eval(src).unwrap_err();
    eprintln!("{err}");
    assert!(format!("{}", err).contains("infer error: ambiguous variable `f`"));
    assert!(format!("{}", err).contains("candidates: Bar Int => Int -> Int, Foo Int => Int -> Int"));
    assert!(format!("{}", err).contains("hint: use @{Bar Int}.f or @{Foo Int}.f"));
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
    let err = eval(src).unwrap_err();
    eprintln!("{err}");
    assert!(format!("{}", err).contains("infer error: ambiguous variable `f`"));
    assert!(format!("{}", err).contains("candidates: Bar Int => Int -> Int, Foo Int => Int -> Bool"));
    assert!(format!("{}", err).contains("hint: use @{Bar Int}.f or @{Foo Int}.f"));
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
    let (val, sch) = eval(src).unwrap();
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
    let (val, sch) = eval(src).unwrap();
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
    let (val, sch) = eval(src).unwrap();
    assert_eq!(format!("{}", val), "1");
    assert_eq!(format!("{}", sch.pretty()), "Int");
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
    let (val, sch) = eval(src).unwrap();
    assert_eq!(format!("{}", val), "100");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn test_ambiguous_trait_type() {
    let src = r#"
        trait Foo a { f : a -> a; };
        impl Foo Bool { f = \x. x; };
        impl Foo Int { f = \x. x; };
        f
    "#;
    let err = eval(src).unwrap_err();
    // assert!(format!("{}", err).contains("infer error: AmbiguousVariable"));
    eprintln!("{err}");
    assert!(format!("{}", err).contains("infer error: ambiguous variable `f`"));
    assert!(format!("{}", err).contains("candidates: Foo Bool => Bool -> Bool, Foo Int => Int -> Int"));
    assert!(format!("{}", err).contains("hint: use @{Foo Bool}.f or @{Foo Int}.f"));
}
