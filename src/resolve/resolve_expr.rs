use super::*;

// -------------------------------------------------------------
// === expr ===
pub fn resolve_expr(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    expr: &mut Expr,
) -> Result<(), Error> {
    match &mut expr.body {
        ExprBody::App(f, x) => {
            resolve_expr(phox, module, symbol_env, f)?;
            resolve_expr(phox, module, symbol_env, x)
        }
        ExprBody::Abs(pat, e) => {
            let mut symbol_env2 = symbol_env.duplicate();
            resolve_pat(phox, module, &mut symbol_env2, pat)?;
            resolve_expr(phox, module, &mut symbol_env2, e)
        }
        ExprBody::If(cond, e1, e2) => {
            resolve_expr(phox, module, symbol_env, cond)?;
            resolve_expr(phox, module, symbol_env, e1)?;
            resolve_expr(phox, module, symbol_env, e2)
        }
        ExprBody::Match(strut, arms) => {
            resolve_expr(phox, module, symbol_env, strut)?;
            for (pat, e) in arms {
                let mut symbol_env2 = symbol_env.duplicate();
                resolve_pat(phox, module, &mut symbol_env2, pat)?;
                resolve_expr(phox, module, &mut symbol_env2, e)?;
            }
            Ok(())
        }
        ExprBody::Tuple(es) => {
            for e in es {
                resolve_expr(phox, module, symbol_env, e)?;
            }
            Ok(())
        }
        ExprBody::Record(fields) => {
            for (_field, e) in fields {
                resolve_expr(phox, module, symbol_env, e)?;
            }
            Ok(())
        }
        ExprBody::FieldAccess(e, _field) => {
            resolve_expr(phox, module, symbol_env, e)
        }
        ExprBody::TupleAccess(e, _index) => {
            resolve_expr(phox, module, symbol_env, e)
        }
        ExprBody::Block(items) => {
            let mut symbol_env2 = symbol_env.duplicate();
            for item in items {
                resolve_item(phox, module, &mut symbol_env2, item)?;
            }
            Ok(())
        }
        ExprBody::RawTraitRecord(raw) => {
            expr.body = resolve_expr_trait_record(phox, module, symbol_env, raw)?;
            resolve_expr(phox, module, symbol_env, expr)
        }
        ExprBody::Var(ref mut symbol) => {
            resolve_symbol(phox, module, symbol_env, symbol)
        }
        ExprBody::Lit(_) => Ok(()),
    }
}
