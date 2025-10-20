use lalrpop_util::ParseError;

use crate::grammar::ItemListParser;
use crate::grammar::ExprParser;

use crate::interpreter::eval_item;
use crate::syntax::ast::Program;
use crate::syntax::ast::Item;
use crate::syntax::ast::Expr;
use crate::syntax::ast::resolve_item;

use crate::syntax::lexer::Lexer;
use crate::syntax::token::{Token, LexicalError};

use crate::typesys::infer_item;
use crate::typesys::initial_type_env;
// use crate::typesys::initial_kind_env;
use crate::typesys::{Type, Scheme};
use crate::typesys::{TypeContext, infer_expr, generalize};
use crate::interpreter::{initial_env, Value};
use crate::interpreter::eval;

/// Parse an expression.
pub fn parse_expr(src: &str) -> Result<Expr, String> {
    let mut lexer = Lexer::new(src);
    ExprParser::new()
        .parse(&mut lexer)
        .map_err(|e| format!("parse error: {e:?}"))
}

/// Infer type scheme of Expr AST.
pub fn infer_expr_scheme(ast: &Expr) -> Result<Scheme, String> {
    let mut ctx = TypeContext::new();
    let mut env = initial_type_env(&mut ctx);
    let ty = infer_expr(&mut ctx, &mut env, &ast)
        .map_err(|e| format!("infer error: {e:?}"))?;
    let sch = generalize(&mut ctx, &env, &ty);
    Ok(sch)
}

/// Infer type of Expr AST.
pub fn infer_expr_type(ast: &Expr) -> Result<Type, String> {
    let sch = infer_expr_scheme(&ast)?;
    Ok(sch.ty)
}

/// Parse and infer type scheme of an expression.
pub fn check_expr_scheme(src: &str) -> Result<Scheme, String> {
    let ast = parse_expr(src)?;
    infer_expr_scheme(&ast)
}

/// Parse and infer type of an expression.
pub fn check_expr_type(src: &str) -> Result<Type, String> {
    let sch = check_expr_scheme(src)?;
    Ok(sch.ty)
}

/// Parse, infer type scheme, and evaluate of an expression.
pub fn eval_expr(src: &str) -> Result<(Value, Scheme), String> {
    let ast = parse_expr(src)?;
    let sch = infer_expr_scheme(&ast)?;
    let mut env = initial_env();
    let val = eval::eval_expr(&ast, &mut env);
    Ok((val, sch))
}

/// Parse list of items.
pub fn parse_items(src: &str) -> Result<Vec<Item>, ParseError<usize, Token, LexicalError>> {
    let mut lexer = Lexer::new(src);
    ItemListParser::new().parse(&mut lexer)
}

/// Parse a program.
pub fn parse_program(src: &str) -> Result<Program, ParseError<usize, Token, LexicalError>> {
    parse_items(src)
}

/// Parse, infer type scheme, and evaluate of a program.
pub fn eval_program(src: &str) -> Result<(Value, Scheme), String> {
    let tops = parse_program(src)
        .map_err(|e| format!("parse error: {e:?}"))?;

    // let mut kenv = initial_kind_env();
    let mut ctx = TypeContext::new();
    let mut tenv = initial_type_env(&mut ctx);
    let mut env = initial_env();

    let mut last = None;
    for item in tops {
        resolve_item(&mut ctx, &mut tenv, &mut env, &item);
        let ty = infer_item(&mut ctx, &mut tenv, &item)
            .map_err(|e| format!("infer error: {e:?}"))?;
        let sch = generalize(&mut ctx, &tenv, &ty);
        let val = eval_item(&item, &mut env);
        last = Some((val, sch));
    }
    last.ok_or_else(|| "program contained no expression".to_string())
}
