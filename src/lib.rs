pub mod api;
pub mod syntax;
pub mod typesys;
pub mod interpreter;
pub mod repl;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub grammar); // grammar.lalrpop を読み込む
