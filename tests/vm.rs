use phox::vm::*;
use phox::module::*;

fn symbol<S: Into<String>>(s: S) -> Symbol {
    let s = s.into();
    if s.starts_with("::") {
        let xs: Vec<_> = s.split("::").collect();
        Symbol::Unique(Path::absolute(xs[1..].to_vec()))
    }
    else {
        Symbol::local(s)
    }
}

fn globals() -> GlobalEnv {
    fn add_unary_op(global: &mut GlobalEnv, prim_sym: &str, prim: Builtin, sym: &str) {
        global.insert(
            symbol(prim_sym),
            Code::Builtin(prim)
        );
        global.insert(
            symbol(sym),
            Code::GlobalVar(symbol(prim_sym))
        );
    }

    fn add_binary_op(global: &mut GlobalEnv, prim_sym: &str, prim: Builtin, sym: &str) {
        global.insert(
            symbol(prim_sym),
            Code::Builtin(prim)
        );
        global.insert(
            symbol(sym),
            Code::lam(Code::lam(
                Code::app(
                    Code::GlobalVar(symbol(prim_sym)),
                    Code::Tuple(2)
                )
            ))
        );
    }

    let mut global = GlobalEnv::new();

    add_unary_op(&mut global, "::core::__i64_neg__", Builtin::I64Neg, "negate");

    add_binary_op(&mut global, "::core::__i64_add__", Builtin::I64Add, "+");
    add_binary_op(&mut global, "::core::__i64_sub__", Builtin::I64Sub, "-");
    add_binary_op(&mut global, "::core::__i64_mul__", Builtin::I64Mul, "*");
    add_binary_op(&mut global, "::core::__i64_div__", Builtin::I64Div, "/");
    add_binary_op(&mut global, "::core::__i64_mod__", Builtin::I64Mod, "%");

    add_binary_op(&mut global, "::core::__i64_eq__", Builtin::I64Eq , "==");
    add_binary_op(&mut global, "::core::__i64_ne__", Builtin::I64Neq, "!=");

    add_binary_op(&mut global, "::core::__i64_le__", Builtin::I64Le, "<=");
    add_binary_op(&mut global, "::core::__i64_lt__", Builtin::I64Lt, "<");
    add_binary_op(&mut global, "::core::__i64_ge__", Builtin::I64Ge, ">=");
    add_binary_op(&mut global, "::core::__i64_gt__", Builtin::I64Gt, ">");


    global
}

pub fn tuple2(t1: Code, t2: Code) -> Code {
    Code::clo(Code::Tuple(2), vec![t1, t2])
}

pub fn record(ix: Vec<Label>, xs: Vec<Code>) -> Code {
    Code::clo(Code::Record(ix), xs)
}

pub fn cons(x: Code, xs: Code) -> Code {
    Code::clo(Code::Con(Symbol::local("Cons"), 2), vec![x, xs])
}

pub fn nil() -> Code {
    Code::Con(Symbol::local("Nil"), 0)
}

#[test]
fn test_literal() {
    let code = Code::int(42);

    let result = VM::run(&GlobalEnv::new(), code).unwrap();

    assert_eq!(result.value(), &Value::I64(42));
}

// (\x. x) 10
#[test]
fn test_identity_function() {
    let lam = Code::lam(Code::Var(0));
    let arg = Code::int(10);
    let app = Code::app(lam, arg);

    let result = VM::run(&GlobalEnv::new(), app).unwrap();

    assert_eq!(result.value(), &Value::I64(10));
}

// let x = 10 in x
// => Let(10, x)
#[test]
fn test_let() {
    let t1 = Code::int(10);
    let t2 = Code::Var(0);
    let code = Code::let_(t1, t2);

    let result = VM::run(&GlobalEnv::new(), code).unwrap();

    assert_eq!(result.value(), &Value::I64(10));
}

#[test]
fn test_tuple() {
    let t = tuple2(Code::int(1), Code::int(2));

    let result = VM::run(&GlobalEnv::new(), t).unwrap();

    assert_eq!(result, Term::Val(Value::Tuple(vec![Term::Val(Value::I64(1)), Term::Val(Value::I64(2))])));
    assert_eq!(result.to_string(), "(1, 2)".to_string());
}

#[test]
fn test_tuple_access() {
    let t = tuple2(Code::int(1), Code::int(2));

    let code = Code::tuple_access(t, 1);

    let result = VM::run(&GlobalEnv::new(), code).unwrap();

    assert_eq!(result.value(), &Value::I64(2));
}

// match 10 with x -> x
#[test]
fn test_match_var() {
    let scrut = Code::int(10);
    let pat = Pat::Var;
    let body = Code::Var(0);

    let code = Code::match_(scrut, vec![(pat, body)]);

    let result = VM::run(&GlobalEnv::new(), code).unwrap();

    assert_eq!(result.value(), &Value::I64(10));
}

// match (1, 2) with (x, y) -> x
#[test]
fn test_match_tuple() {
    let scrut = tuple2(Code::int(1), Code::int(2));

    let pat = Pat::Tuple(vec![Pat::Var, Pat::Var]);
    let body = Code::Var(1);    // x

    let code = Code::match_(scrut, vec![(pat, body)]);

    let result = VM::run(&GlobalEnv::new(), code).unwrap();

    assert_eq!(result.value(), &Value::I64(1));
}

// Cons 1 (Cons 2 Nil)
#[test]
fn test_con() {
    let code = cons(Code::int(1), cons(Code::int(2), nil()));

    let result = VM::run(&GlobalEnv::new(), code).unwrap();

    assert_eq!(result.to_string(), "Cons 1 (Cons 2 Nil)".to_string());
}

#[test]
fn test_record() {
    let labels = vec!["x".to_string(), "y".to_string()];
    let values = vec![Code::int(1), Code::int(2)];
    let code = record(labels.clone(), values);

    let result = VM::run(&GlobalEnv::new(), code).unwrap();

    assert_eq!(result, Term::Val(Value::Record(labels, vec![Term::Val(Value::I64(1)), Term::Val(Value::I64(2))])));
    assert_eq!(result.to_string(), "@{ x = 1, y = 2 }".to_string());
}

// { x = 1, y = 2 }.y
#[test]
fn test_field_access() {
    let labels = vec!["x".to_string(), "y".to_string()];
    let values = vec![Code::int(1), Code::int(2)];
    let record = record(labels.clone(), values);

    let code = Code::field_access(record, "y".to_string());

    let result = VM::run(&GlobalEnv::new(), code).unwrap();

    assert_eq!(result.value(), &Value::I64(2));
}

// global x = 42
// x
#[test]
fn test_global_var() {
    let mut gvars = GlobalEnv::new();
    gvars.insert(
        Symbol::local("x"),
        Code::int(42)
    );

    let code = Code::GlobalVar(Symbol::local("x"));

    let result = VM::run(&gvars, code).unwrap();

    assert_eq!(result.value(), &Value::I64(42));
}

#[test]
fn test_builtin_add() {
    // (add 41 1)
    let code = Code::app(
        Code::Builtin(Builtin::I64Add),
        tuple2(Code::int(41), Code::int(1))
    );

    let result = VM::run(&GlobalEnv::new(), code).unwrap();

    assert_eq!(result.value(), &Value::I64(42));
}

#[test]
fn test_tuple_closure_application() {
    // λx. x
    let lam = Code::lam(Code::Var(0));

    // ( (\x. x), 10 )
    let tuple = tuple2(lam, Code::int(10));

    // ((\x. x), 10).0
    let first = Code::tuple_access(tuple, 0);

    // ((\x. x), 10).0 5
    let app = Code::app(first, Code::int(5));

    let result = VM::run(&GlobalEnv::new(), app).unwrap();

    assert_eq!(result.value(), &Value::I64(5));
}

// ((\x. x), (\y. y)).1  5
// // => 5
#[test]
fn test_nested_tuple_closure_application() {
    // λx. x
    let lam1 = Code::lam(Code::Var(0));
    // λy. y
    let lam2 = Code::lam(Code::Var(0));

    let tuple = tuple2(lam1, lam2);

    // second element: index 1
    let second = Code::tuple_access(tuple, 1);

    // apply to 5
    let app = Code::app(second, Code::int(5));

    let result = VM::run(&GlobalEnv::new(), app).unwrap();

    assert_eq!(result.value(), &Value::I64(5));
}

// @{ f = \x. x, n = 10 }.f 7
// // => 7
#[test]
fn test_record_closure_application() {
    let lam = Code::lam(Code::Var(0));

    let rec = record(
        vec!["f".to_string(), "n".to_string()],
        vec![lam, Code::int(10)],
    );

    let field = Code::field_access(rec, "f".to_string());

    let app = Code::app(field, Code::int(7));

    let result = VM::run(&GlobalEnv::new(), app).unwrap();

    assert_eq!(result.value(), &Value::I64(7));
}

// match (Cons (\x. x) Nil) {
//     Cons f n => f 3,
// }
// // => 3
#[test]
fn test_con_closure_application() {
    let lam = Code::lam(Code::Var(0));

    let scrut = cons(lam, nil());

    let pat = Pat::Con(
        Symbol::local("Cons"),
        vec![Pat::Var, Pat::Var],
    );

    // f 3  → Var(1) 3
    let body = Code::app(Code::Var(1), Code::int(3));

    let code = Code::match_(scrut, vec![(pat, body)]);

    let result = VM::run(&GlobalEnv::new(), code).unwrap();

    assert_eq!(result.value(), &Value::I64(3));
}

// let f = \x. x in
// (f, 99).0 123
// => 123
#[test]
fn test_strict_let_in_tuple_closure() {
    let lam = Code::lam(Code::Var(0));

    let tuple = tuple2(Code::Var(0), Code::int(99));

    let access = Code::tuple_access(tuple, 0);

    let app = Code::app(access, Code::int(123));

    let code = Code::let_(lam, app);

    let result = VM::run(&GlobalEnv::new(), code).unwrap();

    assert_eq!(result.value(), &Value::I64(123));
}

// {()}
#[test]
fn test_empty_block() {
    let code = Code::block(vec![]);

    let result = VM::run(&GlobalEnv::new(), code).unwrap();

    assert_eq!(result.value(), &Value::Unit);
}

// { 1; 2 }
#[test]
fn test_simple_sequence_block() {
    let code = Code::block(vec![Code::int(1), Code::int(2)]);

    let result = VM::run(&GlobalEnv::new(), code).unwrap();

    assert_eq!(result.value(), &Value::I64(2));
}

// { let x = 1; let y = 2; x + y }
#[test]
fn test_block_with_expr_tail() {
    let code = Code::let_(
        Code::int(1),
        Code::let_(
            Code::int(2),
            Code::app(
                Code::Builtin(Builtin::I64Add),
                tuple2(Code::Var(1), Code::Var(0)),
            )
        )
    );

    let result = VM::run(&GlobalEnv::new(), code).unwrap();

    assert_eq!(result.value(), &Value::I64(3));
}

// { let x = 1; { x } }
#[test]
fn test_block_can_see_outer_scope() {
    let code = Code::let_(Code::int(1), Code::block(vec![Code::Var(0)]));

    let result = VM::run(&GlobalEnv::new(), code).unwrap();

    assert_eq!(result.value(), &Value::I64(1));
}

// let x = 1; { let y = 2; y }; x
#[test]
fn test_inner_scope_does_not_escape() {
    // inner: let y = 2 in Var(0)
    let inner = Code::let_(Code::int(2), Code::Var(0));

    // block: { inner }
    let block = Code::block(vec![inner]);

    // seq: let _ = block in x
    let seq = Code::let_(block, Code::Var(1));

    // outer: let x = 1 in seq
    let code = Code::let_(Code::int(1), seq);

    let result = VM::run(&GlobalEnv::new(), code).unwrap();

    assert_eq!(result.value(), &Value::I64(1));
}

// let x = 1 in { let x = 2 in { x } }
#[test]
fn test_nested_block_shadowing() {
    // inner-most: { Var(0) }
    let inner = Code::block(vec![Code::Var(0)]);

    // middle: let x = 2 in inner
    let middle = Code::let_(Code::int(2), inner);

    // outer block: { middle }
    let block = Code::block(vec![middle]);

    // outer-most: let x = 1 in block
    let code = Code::let_(Code::int(1), block);

    let result = VM::run(&GlobalEnv::new(), code).unwrap();

    assert_eq!(result.value(), &Value::I64(2));
}

#[test]
fn test_for_loop() {
    // init = 0;
    let init = Code::int(0);

    // pred = \x. x < 99999;
    let pred = Code::lam(
        Code::app(Code::app(
            Code::GlobalVar(symbol("<")), Code::Var(0)), Code::int(99999)
        )
    );

    // next = \x. x + 2;
    let next = Code::lam(
        Code::app(Code::app(
            Code::GlobalVar(symbol("+")), Code::Var(0)), Code::int(2)
        )
    );

    let coce = Code::for_(init, pred, next);

    let result = VM::run(&globals(), coce).unwrap();

    assert_eq!(result.value(), &Value::I64(100000));
}

#[test]
fn test_for_loop2() {
    // faster version of test_for_loop

    // init = 0;
    let init = Code::int(0);

    // pred = \x. 99999 > x;
    //      = |(99999 >);
    let pred = Code::app(Code::GlobalVar(symbol(">")), Code::int(99999));

    // next = \x. 2 + x;
    //      = |(2 +);
    let next = Code::app(Code::GlobalVar(symbol("+")), Code::int(2));

    let code = Code::for_(init, pred, next);

    let result = VM::run(&globals(), code).unwrap();

    assert_eq!(result.value(), &Value::I64(100000));
}

#[test]
fn test_add_literals() {
    let code = Code::app(Code::app(
        Code::GlobalVar(symbol("+")), Code::int(1)), Code::int(2)
    );

    let result = VM::run(&globals(), code).unwrap();

    assert_eq!(result.value(), &Value::I64(3));
}

#[test]
fn test_for_loop_tuple() {

    // init = (5, 1);
    let init = tuple2(Code::int(5), Code::int(1));

    // pred = \(n, _). 0 < n;
    //      = \x. match (x) { (n, _) => 0 < n };
    let pred = {
        let pat = Pat::Tuple(vec![Pat::Var, Pat::Wildcard]);
        let body = Code::app(Code::app(
            Code::GlobalVar(symbol("<")), Code::int(0)), Code::Var(0)
        );
        Code::lam(
            Code::match_(Code::Var(0), vec![
                (pat, body)
            ])
        )
    };

    // next = \(n, a). (n-1, n*a);
    //      = \x. match (x) { (n, a) => (n-1, n+a) };
    let next = {
        let pat = Pat::Tuple(vec![Pat::Var, Pat::Var]);
        let body = tuple2(
            Code::app(Code::app(
                Code::GlobalVar(symbol("-")), Code::Var(1)), Code::int(1)
            ),
            Code::app(Code::app(
                Code::GlobalVar(symbol("*")), Code::Var(1)), Code::Var(0)
            ),
        );
        Code::lam(
            Code::match_(Code::Var(0), vec![
                (pat, body)
            ])
        )
    };

    let code = Code::tuple_access(Code::for_(init, pred, next), 1);

    let result = VM::run(&globals(), code).unwrap();

    assert_eq!(result.value(), &Value::I64(120));
}

#[test]
fn test_add_div() {
    let a = Code::app(Code::app(
        Code::GlobalVar(symbol("+")), Code::int(2)), Code::int(3)
    );
    let b = Code::app(Code::app(
        Code::GlobalVar(symbol("+")), Code::int(1)), Code::int(1)
    );
    let code = Code::app(Code::app(
        Code::GlobalVar(symbol("/")), a), b
    );

    let result = VM::run(&globals(), code).unwrap();

    assert_eq!(result.value(), &Value::I64(2));
}

#[test]
fn test_lambda_with_pattern_arg() {
    let f = Code::lam_p1(
        Pat::Tuple(vec![Pat::Var, Pat::Var]),
        Code::app(Code::app(Code::GlobalVar(symbol("+")), Code::Var(1)), Code::Var(0))
    );
    let x = tuple2(Code::int(42), Code::int(1));
    let code = Code::app(f, x);

    let result = VM::run(&globals(), code).unwrap();

    assert_eq!(result.value(), &Value::I64(43));
}

#[test]
fn test_nested_lambda_with_pattern_arg() {
    // f = \a.\(b,c). a * (b + c);
    let f = Code::lam_p1(Pat::Var, Code::lam_p1(Pat::Tuple(vec![Pat::Var, Pat::Var]),
            Code::app(Code::app(
                Code::GlobalVar(symbol("*")), Code::Var(3)), // Hmm...
                      Code::app(Code::app(
                          Code::GlobalVar(symbol("+")), Code::Var(1)), Code::Var(0)
                      )
                )
            )
        );

    let a = Code::int(2);
    let x = tuple2(Code::int(42), Code::int(1));
    let code = Code::app(Code::app(f, a), x);

    let result = VM::run(&globals(), code).unwrap();

    assert_eq!(result.value(), &Value::I64(86));
}

#[test]
fn test_fact() {
    // `let rec fact = \n. if (n <= 0) 1 else n * (fact (n - 1))`
    let fact = {
        let f = Code::Var(1);
        let n = Code::Var(0);
        let cond = Code::app(Code::app(
            Code::GlobalVar(symbol("<=")), n.clone()), Code::int(0)
        );
        let texpr = Code::int(1);
        let fexpr = Code::app(Code::app(
            Code::GlobalVar(symbol("*")), n.clone()), Code::app(
            f, Code::app(Code::app(
                Code::GlobalVar(symbol("-")), n), Code::int(1)
            ))
        );
        Code::lam(Code::if_(cond, texpr, fexpr))
    };

    let code = Code::letrec(fact, Code::app(Code::Var(0), Code::int(5)));

    let result = VM::run(&globals(), code).unwrap();

    assert_eq!(result.value(), &Value::I64(120));
}

#[test]
fn test_fact_global() {
    // `fact = \n. if (n <= 0) 1 else n * (fact (n - 1))`
    let fact = {
        let f = Code::GlobalVar(symbol("fact"));
        let n = Code::Var(0);
        let cond = Code::app(Code::app(
            Code::GlobalVar(symbol("<=")), n.clone()), Code::int(0)
        );
        let texpr = Code::int(1);
        let fexpr = Code::app(Code::app(
            Code::GlobalVar(symbol("*")), n.clone()), Code::app(
            f, Code::app(Code::app(
                Code::GlobalVar(symbol("-")), n), Code::int(1)
            ))
        );
        Code::lam(Code::if_(cond, texpr, fexpr))
    };

    let mut gvars = globals();
    gvars.insert(symbol("fact"), fact);

    let code = Code::app(Code::GlobalVar(symbol("fact")), Code::int(5));

    let result = VM::run(&gvars, code).unwrap();

    assert_eq!(result.value(), &Value::I64(120));
}
