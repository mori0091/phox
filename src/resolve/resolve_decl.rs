use super::*;

// -------------------------------------------------------------
// === decl ===
pub fn resolve_decl(
    phox: &mut PhoxEngine,
    module: &RefModule,
    symbol_env: &mut SymbolEnv,
    decl: &mut Decl,
) -> Result<(), TypeError> {
    match decl {
        Decl::Type(raw) => {
            resolve_decl_type_def(phox, module, symbol_env, raw)
        }
        Decl::Trait(raw) => {
            resolve_decl_trait(phox, module, symbol_env, raw)
        }
        Decl::Impl(raw) => {
            resolve_decl_impl(phox, module, symbol_env, raw)
        }
    }
}
