pub mod api;
pub mod syntax;
pub mod typesys;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub grammar); // grammar.lalrpop を読み込む
