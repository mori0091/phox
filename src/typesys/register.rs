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
    module: &RefModule,
    decl: &mut Decl,
) -> Result<(), Error> {
    match decl {
        Decl::Mod(_, _) |
        Decl::Use(_)    |
        Decl::Trait(_) => {
            Ok(())
        }

        Decl::RawType(_) => {
            unreachable!();
        }
        Decl::NamedType(named) => {
            let icx = &mut phox.get_infer_ctx(module);

            // kind を構築
            let mut kind = Kind::Type;
            for _ in named.params.iter().rev() {
                kind = Kind::Fun(Box::new(Kind::Type), Box::new(kind));
            }
            icx.put_kind(named.name.clone(), kind);

            // 各コンストラクタの型スキームを登録
            for v in named.variants.iter() {
                // Type scheme of the data constructor `v`.
                icx.put_type_scheme(
                    v.name(),
                    v.as_scheme(&named.name, &named.params)
                );
            }

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
