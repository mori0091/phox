use crate::api::PhoxEngine;
use crate::syntax::ast::*;
use super::*;

pub fn solve_item(
    phox: &mut PhoxEngine,
    item: &mut Item,
) -> Result<(), Error> {
    match &mut item.body {
        ItemBody::Decl(decl) => {
            solve_decl(phox, decl)?;
            solve(phox, item.constraints.clone())
        }
        ItemBody::Stmt(_stmt) => {
            solve(phox, item.constraints.clone())
        }
        ItemBody::Expr(_expr) => {
            solve(phox, item.constraints.clone())
        }
    }
}

pub fn solve_decl(
    phox: &mut PhoxEngine,
    decl: &mut Decl,
) -> Result<(), Error> {
    match decl {
        Decl::Mod(_name, opt_items) => {
            let Some(items) = opt_items else { unreachable!() };
            for item in items.iter_mut() {
                solve_item(phox, item)?;
            }
        }
        _ => {}
    }
    Ok(())
}
