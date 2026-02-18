use phox::vm::*;

pub fn let_(x: Term, e: Term) -> Term {
    Term::let_(x, e)
}

pub fn tuple2(t1: Term, t2: Term) -> Term {
    let f = Term::lam(Term::lam(Term::Tuple(2)));
    Term::app(Term::app(f, t1), t2)
}

pub fn record(ix: Vec<Label>, xs: Vec<Term>) -> Term {
    let mut f = Term::Record(ix.clone());
    for _ in 0..ix.len() {
        f = Term::lam(f);
    }
    for x in xs {
        f = Term::app(f, x);
    }
    f
}

pub fn cons(x: Term, xs: Term) -> Term {
    let f = Term::lam(Term::lam(Term::Con(Symbol::local("Cons"), 2)));
    Term::app(Term::app(f, x), xs)
}

pub fn nil() -> Term {
    Term::Con(Symbol::local("Nil"), 0)
}

#[test]
fn test_literal() {
    let term = Term::int(42);

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result.term, Term::int(42));
}

// (\x. x) 10
#[test]
fn test_identity_function() {
    let lam = Term::lam(Term::Var(0));
    let arg = Term::int(10);
    let app = Term::app(lam, arg);

    let mut vm = VM::new(GlobalEnv::new(), app);
    let result = vm.run().unwrap();

    assert_eq!(result.term, Term::int(10));
}

// let x = 10 in x
// => Let(10, x)
#[test]
fn test_let() {
    let t1 = Term::int(10);
    let t2 = Term::Var(0);
    let term = Term::let_(t1, t2);

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result.term, Term::int(10));
}

#[test]
fn test_tuple() {
    let t = tuple2(Term::int(1), Term::int(2));

    let mut vm = VM::new(GlobalEnv::new(), t);
    let result = vm.run().unwrap();

    match result {
        Closure{ term: Term::Tuple(2), env: xs } => {
            assert_eq!(xs.len(), 2);
            assert_eq!(xs[0].borrow().term, Term::int(1));
            assert_eq!(xs[1].borrow().term, Term::int(2));
        }
        _ => panic!("expected tuple"),
    }
}

#[test]
fn test_tuple_access() {
    let t = tuple2(Term::int(1), Term::int(2));

    let term = Term::tuple_access(t, 1);

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result.term, Term::int(2));
}

// match 10 with x -> x
#[test]
fn test_match_var() {
    let scrut = Term::int(10);
    let pat = Pat::Var;
    let body = Term::Var(0);

    let term = Term::match_(scrut, vec![(pat, body)]);

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result.term, Term::int(10));
}

// match (1, 2) with (x, y) -> x
#[test]
fn test_match_tuple() {
    let scrut = tuple2(Term::int(1), Term::int(2));

    let pat = Pat::Tuple(vec![Pat::Var, Pat::Var]);
    let body = Term::Var(1);    // x

    let term = Term::match_(scrut, vec![(pat, body)]);

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result.term, Term::int(1));
}

// Cons 1 (Cons 2 Nil)
#[test]
fn test_con() {
    let term = cons(Term::int(1), cons(Term::int(2), nil()));

    eprintln!("term = {:?}", term);

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    eprintln!("result = {:?}", result);

    match result {
        Closure{ term: Term::Con(name, 2), env: xs } => {
            assert_eq!(name, Symbol::local("Cons"));
            let n = xs.len();
            assert_eq!(xs[n-2].borrow().term, Term::int(1));
            match xs[n-1].borrow().clone() {
                Closure{ term: Term::Con(name, 2), env: xs } => {
                    assert_eq!(name, Symbol::local("Cons"));
                    let n = xs.len();
                    assert_eq!(xs[n-2].borrow().term, Term::int(2));
                    match xs[n-1].borrow().clone() {
                        Closure{ term: Term::Con(name, 0), .. } => {
                            assert_eq!(name, Symbol::local("Nil"));
                        }
                        _ => panic!("expected `Nil`"),
                    }
                }
                _ => panic!("expected `Cons 2 Nil`"),
            }
        }
        _ => panic!("expected `Cons 1 (Cons 2 Nil)`"),
    }
}

#[test]
fn test_record() {
    let labels = vec!["x".to_string(), "y".to_string()];
    let values = vec![Term::int(1), Term::int(2)];
    let term = record(labels.clone(), values.clone());

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    match result {
        Closure{ term: Term::Record(ix), env: xs } => {
            assert_eq!(ix, labels);
            let n = xs.len();
            assert_eq!(xs[n-2].borrow().term, values[0]);
            assert_eq!(xs[n-1].borrow().term, values[1]);
        }
        _ => panic!("expected record"),
    }
}

// { x = 1, y = 2 }.y
#[test]
fn test_field_access() {
    let labels = vec!["x".to_string(), "y".to_string()];
    let values = vec![Term::int(1), Term::int(2)];
    let record = record(labels.clone(), values.clone());

    let term = Term::field_access(record, "y".to_string());

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result.term, Term::int(2));
}

// global x = 42
// x
#[test]
fn test_global_var() {
    let mut global = GlobalEnv::new();
    global.insert(
        Symbol::local("x"),
        Term::int(42)
    );

    let term = Term::GlobalVar(Symbol::local("x"));

    let mut vm = VM::new(global, term);
    let result = vm.run().unwrap();

    assert_eq!(result.term, Term::int(42));
}

#[test]
fn test_builtin_add() {
    // (add 41 1)
    let term = Term::app(
        Term::Builtin(Builtin::I64Add),
        tuple2(Term::int(41), Term::int(1))
    );

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result.term, Term::int(42));
}

#[test]
fn test_tuple_closure_application() {
    // λx. x
    let lam = Term::lam(Term::Var(0));

    // ( (\x. x), 10 )
    let tuple = tuple2(lam, Term::int(10));

    // ((\x. x), 10).0
    let first = Term::tuple_access(tuple, 0);

    // ((\x. x), 10).0 5
    let app = Term::app(first, Term::int(5));

    let mut vm = VM::new(GlobalEnv::new(), app);
    let result = vm.run().unwrap();

    assert_eq!(result.term, Term::int(5));
}

// ((\x. x), (\y. y)).1  5
// // => 5
#[test]
fn test_nested_tuple_closure_application() {
    // λx. x
    let lam1 = Term::lam(Term::Var(0));
    // λy. y
    let lam2 = Term::lam(Term::Var(0));

    let tuple = tuple2(lam1, lam2);

    // second element: index 1
    let second = Term::tuple_access(tuple, 1);

    // apply to 5
    let app = Term::app(second, Term::int(5));

    let mut vm = VM::new(GlobalEnv::new(), app);
    let result = vm.run().unwrap();

    assert_eq!(result.term, Term::int(5));
}

// @{ f = \x. x, n = 10 }.f 7
// // => 7
#[test]
fn test_record_closure_application() {
    let lam = Term::lam(Term::Var(0));

    let rec = record(
        vec!["f".to_string(), "n".to_string()],
        vec![lam, Term::int(10)],
    );

    let field = Term::field_access(rec, "f".to_string());

    let app = Term::app(field, Term::int(7));

    let mut vm = VM::new(GlobalEnv::new(), app);
    let result = vm.run().unwrap();

    assert_eq!(result.term, Term::int(7));
}

// match (Cons (\x. x) Nil) {
//     Cons f n => f 3,
// }
// // => 3
#[test]
fn test_con_closure_application() {
    let lam = Term::lam(Term::Var(0));

    let scrut = cons(lam, nil());

    let pat = Pat::Con(
        Symbol::local("Cons"),
        vec![Pat::Var, Pat::Var],
    );

    // f 3  → Var(1) 3
    let body = Term::app(Term::Var(1), Term::int(3));

    let term = Term::match_(scrut, vec![(pat, body)]);

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result.term, Term::int(3));
}

// let f = \x. x in
// (f, 99).0 123
// => 123
#[test]
fn test_strict_let_in_tuple_closure() {
    let lam = Term::lam(Term::Var(0));

    let tuple = tuple2(Term::Var(0), Term::int(99));

    let access = Term::tuple_access(tuple, 0);

    let app = Term::app(access, Term::int(123));

    let term = Term::let_(lam, app);

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result.term, Term::int(123));
}

// {()}
#[test]
fn test_empty_block() {
    let unit = Term::unit();
    let term = Term::block(unit);

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result.term, Term::unit());
}

// { 1; 2 }
#[test]
fn test_simple_sequence_block() {
    let one = Term::int(1);
    let two = Term::int(2);
    let seq = Term::let_(one, two);
    let term = Term::block(seq);

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result.term, Term::int(2));
}

// { let x = 1; let y = 2; x + y }
#[test]
fn test_block_with_expr_tail() {
    let term = Term::let_(
        Term::int(1),
        Term::let_(
            Term::int(2),
            Term::app(
                Term::Builtin(Builtin::I64Add),
                tuple2(Term::Var(1), Term::Var(0)),
            )
        )
    );

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result.term, Term::int(3));
}

// { let x = 1; { x } }
#[test]
fn test_block_can_see_outer_scope() {
    // inner block: { Var(0) }
    let inner = Term::block(Term::Var(0));

    // outer: let x = 1 in inner
    let term = Term::let_(Term::int(1), inner);

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result.term, Term::int(1));
}

// let x = 1; { let y = 2; y }; x
#[test]
fn test_inner_scope_does_not_escape() {
    // inner: let y = 2 in Var(0)
    let inner = Term::let_(Term::int(2), Term::Var(0));

    // block: { inner }
    let block = Term::block(inner);

    // seq: let _ = block in x
    let seq = Term::let_(block, Term::Var(1));

    // outer: let x = 1 in seq
    let term = Term::let_(Term::int(1), seq);

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result.term, Term::int(1));
}

// let x = 1 in { let x = 2 in { x } }
#[test]
fn test_nested_block_shadowing() {
    // inner-most: { Var(0) }
    let inner = Term::block(Term::Var(0));

    // middle: let x = 2 in inner
    let middle = Term::let_(Term::int(2), inner);

    // outer block: { middle }
    let block = Term::block(middle);

    // outer-most: let x = 1 in block
    let term = Term::let_(Term::int(1), block);

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result.term, Term::int(2));
}

#[test]
fn test_for_loop() {
    let init = Term::int(0);

    // pred = \x. x < 99999;
    let pred = Term::lam(Term::app(
        Term::Builtin(Builtin::I64Lt),
        tuple2(Term::Var(0), Term::int(99999))
    ));

    // next = \x. x + 2;
    let next = Term::lam(Term::app(
        Term::Builtin(Builtin::I64Add),
        tuple2(Term::Var(0), Term::int(2))
    ));

    let term = Term::for_(init, pred, next);

    eprintln!("term: {:?}", term);

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result.term, Term::int(100000));
}

pub fn i64_lt(a: Term, b: Term) -> Term {
    // (<) = \x.\y. __i64_le__ (x, y);
    let f = Term::lam(Term::lam(
        Term::app(
            Term::Builtin(Builtin::I64Lt),
            tuple2(Term::Var(1), Term::Var(0))
        )
    ));
    // (<) a b
    Term::app(Term::app(f, a), b)
}

pub fn i64_add(a: Term, b: Term) -> Term {
    // (+) = \x.\y. __i64_add__ (x, y);
    let f = Term::lam(Term::lam(
        Term::app(
            Term::Builtin(Builtin::I64Add),
            tuple2(Term::Var(1), Term::Var(0))
        )
    ));
    // (+) a b
    Term::app(Term::app(f, a), b)
}

pub fn i64_sub(a: Term, b: Term) -> Term {
    // (-) = \x.\y. __i64_sub__ (x, y);
    let f = Term::lam(Term::lam(
        Term::app(
            Term::Builtin(Builtin::I64Sub),
            tuple2(Term::Var(1), Term::Var(0))
        )
    ));
    // (-) a b
    Term::app(Term::app(f, a), b)
}

pub fn i64_mul(a: Term, b: Term) -> Term {
    // (*) = \x.\y. __i64_mul__ (x, y);
    let f = Term::lam(Term::lam(
        Term::app(
            Term::Builtin(Builtin::I64Mul),
            tuple2(Term::Var(1), Term::Var(0))
        )
    ));
    // (*) a b
    Term::app(Term::app(f, a), b)
}

#[test]
fn test_add_literals() {
    let term = Term::app(
        Term::Builtin(Builtin::I64Add),
        tuple2(Term::int(1), Term::int(2)),
    );

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    assert_eq!(result.term, Term::int(3));
}

#[test]
fn test_for_loop_tuple() {

    // init = (5, 1);
    let init = tuple2(Term::int(5), Term::int(1));

    // pred = \(n, _). 0 < n;
    //      = \x. match (x) { (n, _) => 0 < n };
    let pred = {
        let pat = Pat::Tuple(vec![Pat::Var, Pat::Wildcard]);
        let body = i64_lt(Term::int(0), Term::Var(0));
        Term::lam(
            Term::match_(Term::Var(0), vec![
                (pat, body)
            ])
        )
    };

    // next = \(n, a). (n-1, n*a);
    //      = \x. match (x) { (n, a) => (n-1, n+a) };
    let next = {
        let pat = Pat::Tuple(vec![Pat::Var, Pat::Var]);
        let body = tuple2(
            i64_sub(Term::Var(1), Term::int(1)),
            i64_mul(Term::Var(1), Term::Var(0))
        );
        Term::lam(
            Term::match_(Term::Var(0), vec![
                (pat, body)
            ])
        )
    };

    let term = Term::for_(init, pred, next);

    let mut vm = VM::new(GlobalEnv::new(), term);
    let result = vm.run().unwrap();

    let result = result.env[result.env.len() - 1].borrow().term.clone();

    assert_eq!(result, Term::int(120));
}
