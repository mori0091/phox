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
        match api::parse(&buffer) {
            Ok(items) => {
                let mut module = phox.roots.get(api::DEFAULT_USER_ROOT_MODULE_NAME).unwrap();
                for mut item in items {
                    match phox.eval_item(&mut module, &mut item) {
                        Err(e)         => println!("{}", e),
                        Ok((val, sch)) => println!("=> {}: {}", val, sch.pretty()),
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
