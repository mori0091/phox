use crate::api;

use crate::resolve::resolve_item;

use crate::typesys::{infer_item, generalize};
use crate::typesys::apply_trait_impls_item;

use crate::interpreter::eval_item;

use lalrpop_util::ParseError;

pub fn repl() {
    let mut boot = api::Bootstrap::new();
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

        if buffer.starts_with(':') {
            let res = handle_command(&buffer);
            buffer.clear();
            if let Some(src) = res {
                buffer.push_str(&src);
            }
            else {
                continue;
            }
        }
        match api::parse_items(&buffer) {
            Ok(items) => {
                for mut item in items {
                    if let Err(e) = resolve_item(&mut boot.ctx, &mut boot.icx, &mut boot.impl_env, &mut boot.env, &mut item) {
                        println!("resolve error: {}", e);
                    }
                    else {
                        match infer_item(&mut boot.ctx, &mut boot.icx, &mut item) {
                            Err(e) => {
                                println!("infer error: {}", e);
                            }
                            Ok(ty) => {
                                if let Err(e) = apply_trait_impls_item(&mut item, &mut boot.ctx, &boot.icx, &boot.impl_env) {
                                    println!("infer error: {}", e);
                                }
                                else {
                                    // eprintln!("** ImplEnv **\n {:?}\n**", impl_env);
                                    // eprintln!("** obligations **\n {:?}\n**", icx.obligations);
                                    let sch = generalize(&mut boot.ctx, &boot.icx, &ty);
                                    let val = eval_item(&item, &mut boot.env);
                                    println!("=> {}: {}", val, sch.pretty());
                                }
                            }
                        }
                    }
                }
                println!();
                buffer.clear();
                prompt = "> ";
            }
            Err(ParseError::UnrecognizedEof { location: _, expected: _ }) => {
                // 続き待ち
                prompt = "| ";
            }
            Err(e) => {
                println!();
                println!("parse error: {:?}", e);
                buffer.clear();
                prompt = "> ";
            }
        }
    }
}

fn handle_command(input: &str) -> Option<String> {
    let tokens: Vec<&str> = input.trim_start_matches(':').split_whitespace().collect();
    match tokens.as_slice() {
        ["quit"] | ["q"]  => {
            exit();
            None
        }
        ["help"] | ["h"] | ["?"] => {
            help();
            None
        }
        ["load", path] | ["l", path] => {
            load_file(path)
        }
        ["load"] | ["l"] => {
            println!("Required an argument: {}", input);
            None
        }
        _ => {
            println!("Unknown command: {}", input);
            None
        }
    }
}

fn exit() {
    println!();
    std::process::exit(0)
}

fn help() {
    println!(r#"
:quit, :q
    exit REPL.

:help, :h, or :?
    print this help messages.

:load <path>, :l <path>
    load and evaluate Phox source file specified by <path>.

"#
    );
}

fn load_file(path: &str) -> Option<String> {
    match std::fs::read_to_string(path) {
        Err(e) => {
            println!("failed to read {}: {}", path, e);
            println!("");
            None
        }
        Ok(src) => {
            Some(src)
        }
    }
}

fn _repl_todo() {
    println!("Command not implemented yet.");
    println!()
}
