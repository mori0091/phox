use std::io::{self, Read};

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let source = if args.len() >= 2 {
        std::fs::read_to_string(&args[1])
            .unwrap_or_else(|e| {
                eprintln!("failed to read {}: {}", args[1], e);
                std::process::exit(1);
            })
    } else {
        let mut buf = String::new();
        io::stdin().read_to_string(&mut buf).unwrap();
        buf
    };

    match phox::api::eval_program(&source) {
        Ok((val, sch)) => {
            println!("=> {}: {}", val, sch.pretty())
        }
        Err(e) => {
            eprintln!("=> {}", e)
        }
    }
}
