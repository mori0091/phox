use super::*;

// -------------------------------------------------------------
// === expr ===
pub fn resolve_expr(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    param_map: &mut TyParMap,
    expr: &mut Expr,
) -> Result<(), Error> {
    match &mut expr.body {
        ExprBody::App(f, x) => {
            resolve_expr(phox, module, symbol_env, param_map, f)?;
            resolve_expr(phox, module, symbol_env, param_map, x)
        }
        ExprBody::Abs(pat, e) => {
            let mut symbol_env2 = symbol_env.duplicate();
            resolve_pat(phox, module, &mut symbol_env2, pat)?;
            resolve_expr(phox, module, &mut symbol_env2, param_map, e)
        }
        ExprBody::If(cond, e1, e2) => {
            resolve_expr(phox, module, symbol_env, param_map, cond)?;
            resolve_expr(phox, module, symbol_env, param_map, e1)?;
            resolve_expr(phox, module, symbol_env, param_map, e2)
        }
        ExprBody::Match(strut, arms) => {
            resolve_expr(phox, module, symbol_env, param_map, strut)?;
            for (pat, e) in arms {
                let mut symbol_env2 = symbol_env.duplicate();
                resolve_pat(phox, module, &mut symbol_env2, pat)?;
                resolve_expr(phox, module, &mut symbol_env2, param_map, e)?;
            }
            Ok(())
        }
        ExprBody::For(init, pred, next) => {
            resolve_expr(phox, module, symbol_env, param_map, init)?;
            resolve_expr(phox, module, symbol_env, param_map, pred)?;
            resolve_expr(phox, module, symbol_env, param_map, next)
        }
        ExprBody::Builtin(_) => {
            Ok(())
        }
        ExprBody::Con(name, es) => {
            resolve_symbol(phox, module, symbol_env, name)?;
            for e in es {
                resolve_expr(phox, module, symbol_env, param_map, e)?;
            }
            Ok(())
        }
        ExprBody::Tuple(es) => {
            for e in es {
                resolve_expr(phox, module, symbol_env, param_map, e)?;
            }
            Ok(())
        }
        ExprBody::Record(fields) => {
            for (_field, e) in fields {
                resolve_expr(phox, module, symbol_env, param_map, e)?;
            }
            Ok(())
        }
        ExprBody::FieldAccess(e, _field) => {
            resolve_expr(phox, module, symbol_env, param_map, e)
        }
        ExprBody::TupleAccess(e, _index) => {
            resolve_expr(phox, module, symbol_env, param_map, e)
        }
        ExprBody::Block(items) => {
            let mut symbol_env2 = symbol_env.duplicate();
            for item in items {
                resolve_item(phox, module, &mut symbol_env2, param_map, item)?;
            }
            Ok(())
        }
        ExprBody::RawTraitRecord(raw) => {
            let head = resolve_impl_head(phox, module, symbol_env, param_map, &raw)?;
            expr.body = ExprBody::TraitRecord(head);
            resolve_expr(phox, module, symbol_env, param_map, expr)
        }
        ExprBody::TraitRecord(_) => {
            Ok(())
        }
        ExprBody::Var(ref mut symbol) => {
            resolve_symbol(phox, module, symbol_env, symbol)
        }
        ExprBody::Lit(_) => Ok(()),
    }
}
