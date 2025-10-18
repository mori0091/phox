use crate::grammar::ProgramParser;
use crate::grammar::ExprParser;

use crate::interpreter::eval_stmt;
use crate::syntax::ast::{Program, TopLevel};
use crate::syntax::ast::{Item, Expr};
use crate::syntax::ast::resolve_raw_type_decl;
use crate::syntax::ast::register_type_decl;

use crate::typesys::infer_stmt;
use crate::typesys::{initial_kind_env, initial_type_env};
use crate::typesys::{Type, Scheme};
use crate::typesys::{TypeContext, infer_expr, generalize};
use crate::interpreter::{initial_env, Value};
use crate::interpreter::eval;

/// Parse an expression.
pub fn parse_expr(src: &str) -> Result<Expr, String> {
    ExprParser::new().parse(src).map_err(|e| format!("parse error: {e}"))
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

/// Parse a program.
pub fn parse_program(src: &str) -> Result<Program, String> {
    ProgramParser::new()
        .parse(src)
        .map_err(|e| format!("parse error: {e}"))
}

/// Parse, infer type scheme, and evaluate of a program.
pub fn eval_program(src: &str) -> Result<(Value, Scheme), String> {
    let tops = parse_program(src)?;

    let mut kenv = initial_kind_env();
    let mut ctx = TypeContext::new();
    let mut tenv = initial_type_env(&mut ctx);
    let mut env = initial_env();

    let mut last = None;
    for top in tops {
        match top {
            TopLevel::TypeDecl(raw) => {
                let tydecl = resolve_raw_type_decl(&mut ctx, raw);
                register_type_decl(&tydecl, &mut kenv, &mut tenv, &mut env);
            }
            TopLevel::Item(item) => {
                match item {
                    Item::Stmt(stmt) => {
                        let ty = infer_stmt(&mut ctx, &mut tenv, &stmt)
                            .map_err(|e| format!("infer error: {e:?}"))?;
                        let _sch = generalize(&mut ctx, &tenv, &ty);
                        let _val = eval_stmt(&stmt, &mut env);
                        // last = Some((val, sch));
                    }
                    Item::Expr(expr) => {
                        let ty = infer_expr(&mut ctx, &mut tenv, &expr)
                            .map_err(|e| format!("infer error: {e:?}"))?;
                        let sch = generalize(&mut ctx, &tenv, &ty);
                        let val = eval::eval_expr(&expr, &mut env);
                        last = Some((val, sch));
                    }
                }
            }
        }
    }
    last.ok_or_else(|| "program contained no expression".to_string())
}
