use crate::api::PhoxEngine;
use crate::syntax::ast::*;
use crate::module::*;
use super::*;

pub fn register_item(
    phox: &mut PhoxEngine,
    module: &RefModule,
    item: &mut Item,
) -> Result<(), Error> {
    match &mut item.body {
        ItemBody::Decl(decl) => {
            register_decl(phox, module, decl)
        }
        ItemBody::Stmt(_stmt) => {
            Ok(())
        }
        ItemBody::Expr(_expr) => {
            Ok(())
        }
    }
}

pub fn register_decl(
    phox: &mut PhoxEngine,
    _module: &RefModule,
    decl: &mut Decl,
) -> Result<(), Error> {
    match decl {
        Decl::Mod(_, _) |
        Decl::Use(_)    |
        Decl::Type(_)   |
        Decl::Trait(_) => {
            Ok(())
        }

        Decl::RawImpl(_) => {
            unreachable!();
        }
        Decl::NamedImpl(_) => {
            unreachable!();
        }
        Decl::SchImpl(scheme) => {
            let sch_tmpl = SchemeTemplate::new(scheme.clone());
            if !phox.impl_env.insert(sch_tmpl.clone()) {
                let it = sch_tmpl.scheme_ref().target.head.clone();
                return Err(Error::ConflictImpl { it });
            }
            Ok(())
        }

        Decl::RawStarlet(_) => {
            unreachable!();
        }
        Decl::NamedStarlet(_) => {
            unreachable!();
        }
        Decl::SchStarlet(scheme) => {
            let sch_tmpl = SchemeTemplate::new(scheme.clone());
            if !phox.starlet_env.insert(sch_tmpl.clone()) {
                let it = sch_tmpl.scheme_ref().clone();
                return Err(Error::ConflictStarlet { it });
            }
            Ok(())
        }
    }
}
