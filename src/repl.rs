use crate::api;

use lalrpop_util::ParseError;

pub fn repl() {
    let mut phox = api::PhoxEngine::new();
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

        if buffer.starts_with(':') && !buffer.starts_with("::") {
            match handle_command(&mut phox, &buffer) {
                CommandResult::Exit => {
                    println!();
                    std::process::exit(0);
                }
                CommandResult::Continue => {
                    buffer.clear();
                    println!();
                    continue;
                }
                CommandResult::Load(src) => {
                    buffer.clear();
                    buffer.push_str(&src);
                }
            }
        }
        match api::parse(&buffer) {
            Ok(mut items) => {
                let mut module = phox.roots.get(api::DEFAULT_USER_ROOT_MODULE_NAME).unwrap();
                if let Err(e) = phox.resolve_items(&mut module, &mut items) {
                    println!("{}", e)
                }
                else {
                    for mut item in items {
                        match phox.eval_item(&mut module, &mut item) {
                            Err(e)         => println!("{}", e),
                            Ok((val, sch)) => println!("=> {}: {}", val, sch.pretty()),
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
                println!("parse error: {:?}", e);
                println!();
                buffer.clear();
                prompt = "> ";
            }
        }
    }
}

enum CommandResult {
    Continue,
    Exit,
    Load(String),
}

fn help() {
    println!(r#"
:quit, :q
    exit REPL.

:help, :h, or :?
    print this help messages.

:load <path>, :l <path>
    load and evaluate Phox source file specified by <path>.

:modules, :m
    print list of root modules.

:using, :u
    print list of using aliases for each modules.

:symbols, :s
    print list of symbols for each modules.

:impls, or :impls [options]
    print list of `impl`s.
    options:
        -v, or --verbose
            print also the `impl`s' implementation.
"#
    );
}

fn handle_command(phox: &mut api::PhoxEngine, input: &str) -> CommandResult {
    let tokens: Vec<&str> = input.trim_start_matches(':').split_whitespace().collect();
    match tokens.as_slice() {
        ["quit"] | ["q"] => CommandResult::Exit,

        ["help"] | ["h"] | ["?"] => {
            help();
            CommandResult::Continue
        }

        ["load"] | ["l"] => {
            println!("Required an argument: {}", input);
            CommandResult::Continue
        }
        ["load", path] | ["l", path] => {
            match std::fs::read_to_string(path) {
                Ok(src) => CommandResult::Load(src),
                Err(e) => {
                    println!("failed to read {}: {}", path, e);
                    CommandResult::Continue
                }
            }
        }

        ["modules"] | ["m"] => {
            for name in phox.roots.keys() {
                println!("{}", name);
            }
            CommandResult::Continue
        }
        ["using"] | ["u"] => {
            use crate::module::Symbol;
            for module in phox.roots.values() {
                println!("mod {};", module.borrow().path().pretty());
                let mut aliases = module.borrow().using.clone().into_iter().collect::<Vec<_>>();
                aliases.sort_by_key(|(_alias, path)| path.pretty());
                for (alias, path) in aliases.iter() {
                    let tmp = Symbol::Local(alias.to_string());
                    println!("  use {:<30} as {}", path.pretty(), tmp.pretty());
                }
                println!();
            }
            CommandResult::Continue
        }
        ["symbols"] | ["s"] => {
            for (path, symbol_env) in phox.module_symbol_envs.iter() {
                println!("mod {};", path.pretty());
                let map = symbol_env.clone_map();
                let mut syms = map.iter().collect::<Vec<_>>();
                syms.sort_by_key(|(path, _)| path.pretty());
                for (path, symbol) in syms.iter() {
                    println!("  {:<30} {:?}", path.pretty(), symbol);
                }
                println!();
            }
            {
                println!("GLOBAL");
                let map = phox.global_symbol_env.clone_map();
                let mut syms = map.iter().collect::<Vec<_>>();
                syms.sort_by_key(|(path, _)| path.pretty());
                for (path, symbol) in syms.iter() {
                    println!("  {:<30} {:?}", path.pretty(), symbol);
                }
                println!();
            }
            CommandResult::Continue
        }

        ["impls"] => {
            for (impl_head, _members) in phox.impl_env.iter() {
                println!("impl {}", impl_head.pretty());
            }
            CommandResult::Continue
        }
        ["impls", "--verbose"] | ["impls", "-v"] => {
            for (impl_head, members) in phox.impl_env.iter() {
                println!("impl {} {{", impl_head.pretty());
                for (sym, expr) in members.iter() {
                    println!("  {} = {};", sym.pretty(), expr);
                }
                println!("}};");
                println!();
            }
            CommandResult::Continue
        }
        ["impls", _unknown] => {
            println!("Unknown option: {}", input);
            CommandResult::Continue
        }

        _ => {
            println!("Unknown command: {}", input);
            CommandResult::Continue
        }
    }
}
