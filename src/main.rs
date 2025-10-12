use algo_j::api::eval_program;

fn main() {
    let samples = [
        "42",
        "true",
        "if (false) 1 else 2",
        "let x = 5 ; x + 3",
        "\\y. y",
        "let rec f = \\n. if (0 < n) f (n - 1) else 0 ; f 10",
        "1 ; 2 ; 3 ; 4 ; 5",
        "1 ; 2 ; 3 ; 4 ; 5 ;",
        "()",
        "(())",
        "((),)",
        "((),())",
        "((),(),)",
        "(1)",
        "(1,)",
        "((), true, 1)",
        "((), true, 1,)",
        "Empty {}",
        "Empty @ {}",
        "SomeSt @{val:1}",
        "Point@ {x:1, y:2}",
        "RGB@{ r:100, g: 120, b: 255 }",
    ];

    for src in &samples {
        match eval_program(src) {
            Ok((val, sch)) => {
                println!("{} => {}:{}", src, val, sch.pretty())
            }
            Err(e) => {
                println!("{} => {}", src, e)
            }
        }
    }
}

// // ===== Demo: pretty-print principal types =====
// fn main() {
//     let tests = testcases();
//     for (name, expr) in tests {
//         println!("{}: {}", name, expr);
//         let mut _tenv = initial_type_env();
//         let mut ctx = TypeContext::new();
//         let mut env = initial_env(&mut ctx);
//         match infer(&mut ctx, &mut env, &expr) {
//             Ok(ty) => {
//                 let sch = generalize(&mut ctx, &env, &ty);
//                 println!(" : {}", sch);
//             }
//             Err(e) => {
//                 println!(" : type error: {}", e);
//             }
//         }
//     }

//     println!("{:?}", parse_expr("123"));   // => Ok(LitInt(123))
//     println!("{:?}", parse_expr("foo"));   // => Ok(Var("foo"))
//     println!("{:?}", parse_expr("f x"));   // => Ok(App(Var("f"), Var("x")))
//     println!("{:?}", parse_expr("f x y")); // => Ok(App(App(Var("f"), Var("x")), Var("y")))
//     println!("{:?}", parse_expr("f (g x)")); // => Ok(App(Var("f"), App(Var("g")), Var("x")))
//     println!("{:?}", parse_expr("let x = 1 in x"));
// }

// fn testcases() -> Vec<(&'static str, Expr)> {
//     // 定数関数 K
//     // let k = \x. \y. x in k
//     let expr_k = Expr::let_(
//         "k",
//         Expr::abs("x", Expr::abs("y", Expr::var("x"))),
//         Expr::var("k"),
//     );

//     // S コンビネータ
//     // \x. \y. \z. x z (y z)
//     let expr_s = Expr::abs(
//         "x",
//         Expr::abs(
//             "y",
//             Expr::abs(
//                 "z",
//                 Expr::app(
//                     Expr::app(
//                         Expr::var("x"),
//                         Expr::var("z")
//                     ),
//                     Expr::app(
//                         Expr::var("y"),
//                         Expr::var("z")
//                     )
//                 )
//             )
//         )
//     );

//     // 関数合成 (B コンビネータ)
//     // \f. \g. \x. f (g x)
//     let expr_b = Expr::abs(
//         "f",
//         Expr::abs(
//             "g",
//             Expr::abs(
//                 "x",
//                 Expr::app(
//                     Expr::var("f"),
//                     Expr::app(
//                         Expr::var("g"),
//                         Expr::var("x")
//                     )
//                 )
//             )
//         )
//     );

//     // 二重適用
//     // \f. \x. f (f x)
//     let expr_double = Expr::abs(
//         "f",
//         Expr::abs(
//             "x",
//             Expr::app(
//                 Expr::var("f"),
//                 Expr::app(
//                     Expr::var("f"),
//                     Expr::var("x")
//                 )
//             )
//         )
//     );

//     // ----------------------------
//     // 階乗関数(factorial)
//     // let rec fact = \n.
//     //     if (n == 0) then 1 else n * (fact (n - 1))
//     // in fact
//     let fact_expr = Expr::let_rec(
//         "fact",
//         Expr::abs(
//             "n",
//             Expr::if_(
//                 Expr::app(
//                     Expr::app(
//                         Expr::var("=="),
//                         Expr::var("n")
//                     ),
//                     Expr::LitInt(0)
//                 ),
//                 Expr::LitInt(1),
//                 Expr::app(
//                     Expr::app(
//                         Expr::var("*"),
//                         Expr::var("n")
//                     ),
//                     Expr::app(
//                         Expr::var("fact"),
//                         Expr::app(
//                             Expr::app(
//                                 Expr::var("-"), // ← 引き算もプリミティブに追加
//                                 Expr::var("n")
//                             ),
//                             Expr::LitInt(1)
//                         )
//                     )
//                 )
//             )
//         ),
//         Expr::var("fact"),
//     );

//     // ----------------------------
//     // 無限リスト(ones)
//     // let rec ones = 1 :: ones in ones
//     let ones_expr = Expr::let_rec(
//         "ones",
//         Expr::app(
//             Expr::app(
//                 Expr::var("Cons"),
//                 Expr::LitInt(1),
//             ),
//             Expr::var("ones"),
//         ),
//         Expr::var("ones"),
//     );

//     // ----------------------------
//     // 再帰的恒等関数(自己参照)
//     // let rec id = \x. id x in id
//     let rec_id_expr = Expr::let_rec(
//         "id",
//         Expr::abs("x",
//                   Expr::app(Expr::var("id"),
//                             Expr::var("x"))),
//         Expr::var("id"),
//     );

//     // ----------------------------
//     // map Some [1,2,3]
//     let map_some = Expr::app(
//         Expr::app(
//             Expr::var("map"),
//             Expr::var("Some")
//         ),
//         Expr::app(
//             Expr::app(
//                 Expr::var("Cons"),
//                 Expr::LitInt(1),
//             ),
//             Expr::app(
//                 Expr::app(
//                     Expr::var("Cons"),
//                     Expr::LitInt(2),
//                 ),
//                 Expr::app(
//                     Expr::app(
//                         Expr::var("Cons"),
//                         Expr::LitInt(3),
//                     ),
//                     Expr::var("Nil"),
//                 ),
//             ),
//         ),
//     );

//     // ----------------------------
//     let tests = vec![
//         ("id-id", Expr::let_(
//             "id",
//             Expr::abs("x", Expr::var("x")),
//             Expr::app(
//                 Expr::var("id"),
//                 Expr::var("id"))
//         )),
//         ("omega", Expr::abs(
//             "x",
//             Expr::app(Expr::var("x"), Expr::var("x"))
//         )),
//         ("k", expr_k),
//         ("s", expr_s),
//         ("b", expr_b),
//         ("double", expr_double),

//         // 再帰関数
//         ("fact", fact_expr),
//         ("ones", ones_expr),
//         ("rec_id", rec_id_expr),

//         // データ構築子; Option a
//         ("None", Expr::var("None")),
//         ("Some", Expr::var("Some")),
//         ("Some 1", Expr::app(
//             Expr::var("Some"),
//             Expr::LitInt(1),
//         )),
//         // データ構築子; List a
//         ("Nil", Expr::var("Nil")),
//         ("Cons", Expr::var("Cons")),
//         ("Cons 1 Nil", Expr::app(
//             Expr::app(
//                 Expr::var("Cons"),
//                 Expr::LitInt(1)
//             ),
//             Expr::var("Nil")
//         )),

//         ("map Some [1,2,3]", map_some),
//     ];

//     tests
// }
