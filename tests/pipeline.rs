use phox::api::eval;

#[test]
fn pipe_forward() {
    let src = r#"
        let f = λx. x + 1;
        let g = λx. x * 2;
        3 |> f |> g
    "#;
    let (val, sch) = eval(src).unwrap();
    assert_eq!(format!("{}", val), "8");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn pipe_backward() {
    let src = r#"
        let f = λx. x + 1;
        let g = λx. x * 2;
        g <| f <| 3
    "#;
    let (val, sch) = eval(src).unwrap();
    assert_eq!(format!("{}", val), "8");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn compose_right() {
    let src = r#"
        let f = λx. x + 1;
        let g = λx. x * 2;
        (f >> g) 3
    "#;
    let (val, sch) = eval(src).unwrap();
    assert_eq!(format!("{}", val), "8");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn compose_left() {
    let src = r#"
        let f = λx. x + 1;
        let g = λx. x * 2;
        (g << f) 3
    "#;
    let (val, sch) = eval(src).unwrap();
    assert_eq!(format!("{}", val), "8");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn pipe_and_compose_mix() {
    let src = r#"
        let f = λx. x + 1;
        let g = λx. x * 2;
        3 |> f >> g
    "#;
    let (val, sch) = eval(src).unwrap();
    assert_eq!(format!("{}", val), "8");
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn pipe_forward_associativity() {
    let src = r#"
        let f = λx. x + 1;
        let g = λx. x * 2;
        let h = λx. x - 3;
        4 |> f |> g |> h
    "#;
    let (val, sch) = eval(src).unwrap();
    assert_eq!(format!("{}", val), "7"); // h(g(f(4))) = h(10) = 7
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn pipe_backward_associativity() {
    let src = r#"
        let f = λx. x + 1;
        let g = λx. x * 2;
        let h = λx. x - 3;
        h <| g <| f <| 4
    "#;
    let (val, sch) = eval(src).unwrap();
    assert_eq!(format!("{}", val), "7"); // h(g(f(4))) = h(10) = 7
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn compose_right_associativity() {
    let src = r#"
        let f = λx. x + 1;
        let g = λx. x * 2;
        let h = λx. x - 3;
        let c = f >> g >> h;
        c 4
    "#;
    let (val, sch) = eval(src).unwrap();
    assert_eq!(format!("{}", val), "7"); // h(g(f(4))) = h(10) = 7
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn compose_left_associativity() {
    let src = r#"
        let f = λx. x + 1;
        let g = λx. x * 2;
        let h = λx. x - 3;
        let c = h << g << f;
        c 4
    "#;
    let (val, sch) = eval(src).unwrap();
    assert_eq!(format!("{}", val), "7"); // h(g(f(4))) = h(10) = 7
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn pipe_and_compose_nested() {
    let src = r#"
        let f = λx. x + 1;
        let g = λx. x * 2;
        let h = λx. x - 3;
        4 |> f >> g |> h
    "#;
    let (val, sch) = eval(src).unwrap();
    assert_eq!(format!("{}", val), "7"); // h((g ∘ f)(4)) = h(g(f(4))) = h(10) = 7
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn compose_with_pipe_backward() {
    let src = r#"
        let f = λx. x + 1;
        let g = λx. x * 2;
        let h = λx. x - 3;
        (h << g) <| f <| 4
    "#;
    let (val, sch) = eval(src).unwrap();
    assert_eq!(format!("{}", val), "7"); // (h ∘ g)(f(4)) = h(g(5)) = h(10) = 7
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn mixed_pipe_directions() {
    let src = r#"
        let f = λx. x + 1;
        let g = λx. x * 2;
        f <| 3 |> g
    "#;
    let (val, sch) = eval(src).unwrap();
    assert_eq!(format!("{}", val), "7"); // f(g(3)) = f(6) = 7
    assert_eq!(format!("{}", sch.pretty()), "Int");
}

#[test]
fn mixed_compose_and_pipe() {
    let src = r#"
        let f = λx. x + 1;
        let g = λx. x * 2;
        let h = λx. x - 3;
        f >> g <| 3 |> h
    "#;
    let (val, sch) = eval(src).unwrap();
    assert_eq!(format!("{}", val), "2"); // (f >> g)(h(3)) = g(f(h(3))) = g(f(0)) = g(1) = 2
    assert_eq!(format!("{}", sch.pretty()), "Int");
}
