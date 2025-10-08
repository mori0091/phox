use crate::grammar::ExprParser;
use crate::typesys::infer::{TypeContext, initial_type_env, infer, generalize};
use crate::syntax::ast::{Expr, Type, Scheme};

/// Parse source code of an expression.
pub fn parse_expr(src: &str) -> Result<Expr, String> {
    ExprParser::new().parse(src).map_err(|e| format!("parse error: {e}"))
}

/// Infer type scheme of AST.
pub fn infer_scheme(ast: &Expr) -> Result<Scheme, String> {
    let mut ctx = TypeContext::new();
    let mut env = initial_type_env(&mut ctx);
    let ty = infer(&mut ctx, &mut env, &ast)
        .map_err(|e| format!("infer error: {e:?}"))?;
    let sch = generalize(&mut ctx, &env, &ty);
    Ok(sch)
}

/// Infer type of AST.
pub fn infer_type(ast: &Expr) -> Result<Type, String> {
    let sch = infer_scheme(&ast)?;
    Ok(sch.ty)
}

/// Parse source code and infer type scheme of the that's AST.
pub fn check_scheme(src: &str) -> Result<Scheme, String> {
    let ast = parse_expr(src)?;
    infer_scheme(&ast)
}

/// Parse source code and infer type of the that's AST.
pub fn check(src: &str) -> Result<Type, String> {
    let sch = check_scheme(src)?;
    Ok(sch.ty)
}
