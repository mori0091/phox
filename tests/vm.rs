use phox::vm::*;

#[test]
fn test_literal() {
    let global = GlobalEnv::new();
    let term = Term::V(Value::Lit(Lit::Int(42)));

    let mut vm = VM::new(global, term);
    let result = vm.run().unwrap();

    assert_eq!(result, Term::V(Value::Lit(Lit::Int(42))));
}

// (\x. x) 10
#[test]
fn test_identity_function() {
    let lam = Term::Lam(Box::new(Term::E(Expr::Var(0))));
    let arg = Term::V(Value::Lit(Lit::Int(10)));
    let app = Term::E(Expr::App(Box::new(lam), Box::new(arg)));

    let mut vm = VM::new(GlobalEnv::new(), app);
    let result = vm.run().unwrap();

    assert_eq!(result, Term::V(Value::Lit(Lit::Int(10))));
}

// let x = 10 in x
// => StrictLetIn(10, x)
#[test]
fn test_strict_let_in() {
    let t1 = Term::V(Value::Lit(Lit::Int(10)));
    let t2 = Term::E(Expr::Var(0));
    let term = Term::E(Expr::StrictLetIn(Box::new(t1), Box::new(t2)));

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result, Term::V(Value::Lit(Lit::Int(10))));
}

#[test]
fn test_tuple() {
    let t = Term::E(Expr::Tuple(vec![
        Term::V(Value::Lit(Lit::Int(1))),
        Term::V(Value::Lit(Lit::Int(2))),
    ]));

    let mut vm = VM::new(GlobalEnv::new(), t);
    let result = vm.run().unwrap();

    match result {
        Term::V(Value::Tuple(xs)) => {
            assert_eq!(xs.len(), 2);
            assert_eq!(xs[0], Value::Lit(Lit::Int(1)));
            assert_eq!(xs[1], Value::Lit(Lit::Int(2)));
        }
        _ => panic!("expected tuple"),
    }
}

#[test]
fn test_tuple_access() {
    let tuple = Term::E(Expr::Tuple(vec![
        Term::V(Value::Lit(Lit::Int(1))),
        Term::V(Value::Lit(Lit::Int(2))),
    ]));

    let term = Term::E(Expr::TupleAccess(Box::new(tuple), 1));

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result, Term::V(Value::Lit(Lit::Int(2))));
}

// match 10 with x -> x
#[test]
fn test_match_var() {
    let scrut = Term::V(Value::Lit(Lit::Int(10)));
    let pat = Pat::Var;
    let body = Term::E(Expr::Var(0));

    let term = Term::E(Expr::Match(Box::new(scrut), vec![(pat, body)]));

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result, Term::V(Value::Lit(Lit::Int(10))));
}

// match (1, 2) with (x, y) -> x
#[test]
fn test_match_tuple() {
    let scrut = Term::E(Expr::Tuple(vec![
        Term::V(Value::Lit(Lit::Int(1))),
        Term::V(Value::Lit(Lit::Int(2))),
    ]));

    let pat = Pat::Tuple(vec![Pat::Var, Pat::Var]);
    let body = Term::E(Expr::Var(1)); // x

    let term = Term::E(Expr::Match(Box::new(scrut), vec![(pat, body)]));

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result, Term::V(Value::Lit(Lit::Int(1))));
}

// Cons 1 (Cons 2 Nil)
#[test]
fn test_con() {
    let expr = Expr::Con(
        Symbol::local("Cons"),
        vec![
            Term::V(Value::Lit(Lit::Int(1))),
            Term::E(Expr::Con(
                Symbol::local("Cons"),
                vec![
                    Term::V(Value::Lit(Lit::Int(2))),
                    Term::E(Expr::Con(Symbol::local("Nil"), vec![]))
                ]
            ))
        ]
    );

    let list = Value::Con(
        Symbol::local("Cons"),
        vec![
            Value::Lit(Lit::Int(1)),
            Value::Con(
                Symbol::local("Cons"),
                vec![
                    Value::Lit(Lit::Int(2)),
                    Value::Con(Symbol::local("Nil"), vec![])
                ]
            )
        ]
    );

    let term = Term::E(expr.clone());
    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result, Term::V(list));
}

#[test]
fn test_record() {
    let term = Term::E(Expr::Record(vec![
        ("x".to_string(), Term::V(Value::Lit(Lit::Int(1)))),
        ("y".to_string(), Term::V(Value::Lit(Lit::Int(2)))),
    ]));

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    match result {
        Term::V(Value::Record(fs)) => {
            assert_eq!(fs.len(), 2);
            assert_eq!(fs[0], ("x".to_string(), Value::Lit(Lit::Int(1))));
            assert_eq!(fs[1], ("y".to_string(), Value::Lit(Lit::Int(2))));
        }
        _ => panic!("expected record"),
    }
}

// { x = 1, y = 2 }.y
#[test]
fn test_field_access() {
    let record = Term::E(Expr::Record(vec![
        ("x".to_string(), Term::V(Value::Lit(Lit::Int(1)))),
        ("y".to_string(), Term::V(Value::Lit(Lit::Int(2)))),
    ]));

    let term = Term::E(Expr::FieldAccess(Box::new(record), "y".to_string()));

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result, Term::V(Value::Lit(Lit::Int(2))));
}

// global x = 42
// x
#[test]
fn test_global_var() {
    let mut global = GlobalEnv::new();
    global.insert(
        Symbol::local("x"),
        Term::V(Value::Lit(Lit::Int(42)))
    );

    let term = Term::E(Expr::GlobalVar(Symbol::local("x")));

    let mut vm = VM::new(global, term);
    let result = vm.run().unwrap();

    assert_eq!(result, Term::V(Value::Lit(Lit::Int(42))));
}

#[test]
fn test_builtin_add() {
    // (add 41 1)
    let term = Term::E(Expr::App(
        Box::new(Term::Builtin(Builtin::I64Add)),
        Box::new(Term::V(Value::Tuple(vec![
            Value::Lit(Lit::Int(41)),
            Value::Lit(Lit::Int(1))
        ])))
    ));

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result, Term::V(Value::Lit(Lit::Int(42))));
}

#[test]
fn test_tuple_closure_application() {
    // λx. x
    let lam = Term::Lam(Box::new(Term::E(Expr::Var(0))));

    // ( (\x. x), 10 )
    let tuple = Term::E(Expr::Tuple(vec![
        lam,
        Term::V(Value::Lit(Lit::Int(10))),
    ]));

    // ((\x. x), 10).0
    let first = Term::E(Expr::TupleAccess(Box::new(tuple), 0));

    // ((\x. x), 10).0 5
    let app = Term::E(Expr::App(
        Box::new(first),
        Box::new(Term::V(Value::Lit(Lit::Int(5)))),
    ));

    let mut vm = VM::new(GlobalEnv::new(), app);
    let result = vm.run().unwrap();

    assert_eq!(result, Term::V(Value::Lit(Lit::Int(5))));
}

// ((\x. x), (\y. y)).1  5
// // => 5
#[test]
fn test_nested_tuple_closure_application() {
    // λx. x
    let lam1 = Term::Lam(Box::new(Term::E(Expr::Var(0))));
    // λy. y
    let lam2 = Term::Lam(Box::new(Term::E(Expr::Var(0))));

    let tuple = Term::E(Expr::Tuple(vec![
        lam1,
        lam2,
    ]));

    // second element: index 1
    let second = Term::E(Expr::TupleAccess(Box::new(tuple), 1));

    // apply to 5
    let app = Term::E(Expr::App(
        Box::new(second),
        Box::new(Term::V(Value::Lit(Lit::Int(5)))),
    ));

    let mut vm = VM::new(GlobalEnv::new(), app);
    let result = vm.run().unwrap();

    assert_eq!(result, Term::V(Value::Lit(Lit::Int(5))));
}

// @{ f = \x. x, n = 10 }.f 7
// // => 7
#[test]
fn test_record_closure_application() {
    let lam = Term::Lam(Box::new(Term::E(Expr::Var(0))));

    let record = Term::E(Expr::Record(vec![
        ("f".to_string(), lam),
        ("n".to_string(), Term::V(Value::Lit(Lit::Int(10)))),
    ]));

    let field = Term::E(Expr::FieldAccess(Box::new(record), "f".to_string()));

    let app = Term::E(Expr::App(
        Box::new(field),
        Box::new(Term::V(Value::Lit(Lit::Int(7)))),
    ));

    let mut vm = VM::new(GlobalEnv::new(), app);
    let result = vm.run().unwrap();

    assert_eq!(result, Term::V(Value::Lit(Lit::Int(7))));
}

// match (Con (\x. x) 10) {
//     Con f n => f 3,
// }
// // => 3
#[test]
fn test_con_closure_application() {
    let lam = Term::Lam(Box::new(Term::E(Expr::Var(0))));

    let scrut = Term::E(Expr::Con(
        Symbol::local("Con"),
        vec![
            lam,
            Term::V(Value::Lit(Lit::Int(10))),
        ],
    ));

    let pat = Pat::Con(
        Symbol::local("Con"),
        vec![Pat::Var, Pat::Var],
    );

    // f 3  → Var(1) 3
    let body = Term::E(Expr::App(
        Box::new(Term::E(Expr::Var(1))),
        Box::new(Term::V(Value::Lit(Lit::Int(3)))),
    ));

    let term = Term::E(Expr::Match(Box::new(scrut), vec![(pat, body)]));

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result, Term::V(Value::Lit(Lit::Int(3))));
}

// let f = \x. x in
// (f, 99).0 123
// => 123
#[test]
fn test_strict_let_in_tuple_closure() {
    let lam = Term::Lam(Box::new(Term::E(Expr::Var(0))));

    let tuple = Term::E(Expr::Tuple(vec![
        Term::E(Expr::Var(0)), // f
        Term::V(Value::Lit(Lit::Int(99))),
    ]));

    let access = Term::E(Expr::TupleAccess(Box::new(tuple), 0));

    let app = Term::E(Expr::App(
        Box::new(access),
        Box::new(Term::V(Value::Lit(Lit::Int(123)))),
    ));

    let term = Term::E(Expr::StrictLetIn(
        Box::new(lam),
        Box::new(app),
    ));

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result, Term::V(Value::Lit(Lit::Int(123))));
}

// {()}
#[test]
fn test_empty_block() {
    let unit = Term::V(Value::Lit(Lit::Unit));
    let term = Term::E(Expr::Block(Box::new(unit)));

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result, Term::V(Value::Lit(Lit::Unit)));
}

// { 1; 2 }
#[test]
fn test_simple_sequence_block() {
    let one = Term::V(Value::Lit(Lit::Int(1)));
    let two = Term::V(Value::Lit(Lit::Int(2)));
    let seq = Term::E(Expr::StrictLetIn(Box::new(one), Box::new(two)));
    let term = Term::E(Expr::Block(Box::new(seq)));

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result, Term::V(Value::Lit(Lit::Int(2))));
}

// { let x = 1; let y = 2; x + y }
#[test]
fn test_block_with_expr_tail() {
    let expr = Term::E(Expr::App(
        Box::new(Term::Builtin(Builtin::I64Add)),
        Box::new(Term::E(Expr::Tuple(vec![
            Term::E(Expr::Var(1)),
            Term::E(Expr::Var(0))
        ]))),
    ));
    let term = Term::E(Expr::StrictLetIn(
        Box::new(Term::V(Value::Lit(Lit::Int(2)))),
        Box::new(expr),
    ));
    let term = Term::E(Expr::StrictLetIn(
        Box::new(Term::V(Value::Lit(Lit::Int(1)))),
        Box::new(term),
    ));

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result, Term::V(Value::Lit(Lit::Int(3))));
}

// { let x = 1; { x } }
#[test]
fn test_block_can_see_outer_scope() {
    // inner block: { Var(0) }
    let inner = Term::E(Expr::Block(Box::new(
        Term::E(Expr::Var(0))
    )));

    // outer: let x = 1 in inner
    let term = Term::E(Expr::StrictLetIn(
        Box::new(Term::V(Value::Lit(Lit::Int(1)))),
        Box::new(inner),
    ));

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result, Term::V(Value::Lit(Lit::Int(1))));
}

// let x = 1; { let y = 2; y }; x
#[test]
fn test_inner_scope_does_not_escape() {
    // inner: let y = 2 in Var(0)
    let inner = Term::E(Expr::StrictLetIn(
        Box::new(Term::V(Value::Lit(Lit::Int(2)))),
        Box::new(Term::E(Expr::Var(0))),
    ));

    // block: { inner }
    let block = Term::E(Expr::Block(Box::new(inner)));

    // seq: let _ = block in x
    let seq = Term::E(Expr::StrictLetIn(
        Box::new(block),
        Box::new(Term::E(Expr::Var(1))), // x
    ));

    // outer: let x = 1 in seq
    let term = Term::E(Expr::StrictLetIn(
        Box::new(Term::V(Value::Lit(Lit::Int(1)))),
        Box::new(seq),
    ));

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result, Term::V(Value::Lit(Lit::Int(1))));
}

// let x = 1 in { let x = 2 in { x } }
#[test]
fn test_nested_block_shadowing() {
    // inner-most: { Var(0) }
    let inner = Term::E(Expr::Block(Box::new(
        Term::E(Expr::Var(0))
    )));

    // middle: let x = 2 in inner
    let middle = Term::E(Expr::StrictLetIn(
        Box::new(Term::V(Value::Lit(Lit::Int(2)))),
        Box::new(inner),
    ));

    // outer block: { middle }
    let block = Term::E(Expr::Block(Box::new(middle)));

    // outer-most: let x = 1 in block
    let term = Term::E(Expr::StrictLetIn(
        Box::new(Term::V(Value::Lit(Lit::Int(1)))),
        Box::new(block),
    ));

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result, Term::V(Value::Lit(Lit::Int(2))));
}

#[test]
fn test_for_loop() {
    let init = Term::V(Value::Lit(Lit::Int(0)));

    let pred = Term::Lam(Box::new(
        Term::E(Expr::App(Box::new(Term::Builtin(Builtin::I64Lt)),
                          Box::new(Term::E(Expr::Tuple(vec![
                              Term::E(Expr::Var(0)),
                              Term::V(Value::Lit(Lit::Int(99999))),
                          ])))
        ))
    ));

    let next = Term::Lam(Box::new(
        Term::E(Expr::App(Box::new(Term::Builtin(Builtin::I64Add)),
                          Box::new(Term::E(Expr::Tuple(vec![
                              Term::E(Expr::Var(0)),
                              Term::V(Value::Lit(Lit::Int(2))),
                          ])))
        ))
    ));

    let term = Term::E(Expr::For(Box::new(init), Box::new(pred), Box::new(next)));

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result, Term::V(Value::Lit(Lit::Int(100000))));
}
