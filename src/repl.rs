use crate::api;

use crate::typesys::TypeContext;
// use crate::typesys::initial_kind_env;
use crate::typesys::initial_type_env;
use crate::interpreter::initial_env;

use crate::syntax::ast::resolve_item;
use crate::typesys::{infer_item, generalize};
use crate::interpreter::eval_item;

use lalrpop_util::ParseError;

pub fn repl() {
    let mut ctx = TypeContext::new();
    // let mut kenv = initial_kind_env(&mut ctx);
    let mut tenv = initial_type_env(&mut ctx);
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
                for item in items {
                    resolve_item(&mut ctx, &mut tenv, &mut env, &item);
                    match infer_item(&mut ctx, &mut tenv, &item) {
                        Ok(ty) => {
                            let sch = generalize(&mut ctx, &tenv, &ty);
                            let val = eval_item(&item, &mut env);
                            println!("=> {}: {}", val, sch.pretty());
                        }
                        Err(e) => {
                            eprintln!("type error: {:?}", e);
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
