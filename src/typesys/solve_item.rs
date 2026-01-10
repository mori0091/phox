use crate::api::PhoxEngine;
use crate::syntax::ast::*;
use super::*;

pub fn solve_item(
    phox: &mut PhoxEngine,
    item: &mut Item,
) -> Result<(), Error> {
    match &mut item.body {
        ItemBody::Decl(_decl) => {
            solve(phox, item.constraints.clone())
        }
        ItemBody::Stmt(stmt) => {
            solve_stmt(phox, stmt)?;
            solve(phox, item.constraints.clone())
        }
        ItemBody::Expr(_expr) => {
            solve(phox, item.constraints.clone())
        }
    }
}

pub fn solve_stmt(
    phox: &mut PhoxEngine,
    stmt: &mut Stmt,
) -> Result<(), Error> {
    match stmt {
        Stmt::Mod(_name, opt_items) => {
            let Some(items) = opt_items else { unreachable!() };
            for item in items.iter_mut() {
                solve_item(phox, item)?;
            }
        }
        _ => {}
    }
    Ok(())
}
