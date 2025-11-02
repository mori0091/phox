use std::io::{self, Read};

use phox::repl::repl;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() == 1 {
        repl();
    } else {
        let source = if args[1] == "-" {
            let mut buf = String::new();
            io::stdin().read_to_string(&mut buf).unwrap();
            buf
        } else {
            std::fs::read_to_string(&args[1]).unwrap_or_else(|e| {
                eprintln!("failed to read {}: {}", args[1], e);
                std::process::exit(1);
            })
        };

        match phox::api::eval(&source) {
            Ok((val, sch)) => {
                println!("=> {}: {}", val, sch.pretty())
            }
            Err(e) => {
                eprintln!("{}", e);
                std::process::exit(1);
            }
        }
    }
}
