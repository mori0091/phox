use crate::api;

use crate::typesys::{apply_trait_impls_item, ImplEnv, InferCtx, TypeContext};
use crate::interpreter::initial_env;

use crate::syntax::ast::resolve_item;
use crate::typesys::{infer_item, generalize};
use crate::interpreter::eval_item;

use lalrpop_util::ParseError;

pub fn repl() {
    let mut ctx = TypeContext::new();
    let mut icx = InferCtx::initial(&mut ctx);
    let mut impl_env = ImplEnv::new();
    let mut env = initial_env();

    let mut buffer = String::new();
    let mut prompt = "> ";

    loop {
        print!("{prompt}");
        std::io::Write::flush(&mut std::io::stdout()).unwrap();

        let mut line = String::new();
        if std::io::stdin().read_line(&mut line).unwrap() == 0 {
            println!();
            break; // EOF (Ctrl+D)
        }
        buffer.push_str(&line);

        match api::parse_items(&buffer) {
            Ok(items) => {
                for mut item in items {
                    if let Err(e) = resolve_item(&mut ctx, &mut icx, &mut impl_env, &mut env, &item) {
                        eprintln!("resolve error: {:?}", e);
                    }
                    else {
                        match infer_item(&mut ctx, &mut icx, &mut item) {
                            Ok(ty) => {
                                if let Err(e) = apply_trait_impls_item(&mut item, &mut ctx, &icx, &impl_env) {
                                    eprintln!("apply trait impl error: {:?}", e);
                                }
                                else {
                                    // eprintln!("** ImplEnv **\n {:?}\n**", impl_env);
                                    // eprintln!("** obligations **\n {:?}\n**", icx.obligations);
                                    let sch = generalize(&mut ctx, &icx, &ty);
                                    let val = eval_item(&item, &mut env);
                                    println!("=> {}: {}", val, sch.pretty());
                                }
                            }
                            Err(e) => {
                                eprintln!("type error: {:?}", e);
                            }
                        }
                    }
                }
                buffer.clear();
                prompt = "> ";
            }
            Err(ParseError::UnrecognizedEof { location: _, expected: _ }) => {
                // 続き待ち
                prompt = "| ";
            }
            Err(e) => {
                eprintln!("parse error: {:?}", e);
                buffer.clear();
                prompt = "> ";
            }
        }
    }
}
