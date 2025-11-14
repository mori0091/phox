use std::collections::HashMap;

use lalrpop_util::ParseError;
use crate::grammar::*;

use crate::syntax::ast::*;
use crate::syntax::lexer::*;
use crate::syntax::token::*;

use crate::module::*;
use crate::resolve::*;
use crate::typesys::*;
use crate::interpreter::*;

use crate::prelude::*;

pub const DEFAULT_USER_ROOT_MODULE_NAME: &str = "__main__";

pub struct PhoxEngine {
    pub ctx: TypeContext,
    pub roots: RootModules,
    pub global_symbol_env: SymbolEnv,
    pub module_symbol_envs: HashMap<Path, SymbolEnv>,
    pub impl_member_env: TraitMemberEnv, // implメンバの型スキーム集合 (ex. "f": { ∀ Int. Foo Int => Int -> Int, ∀ Bool. Foo Bool => Bool -> Bool })
    pub impl_env: ImplEnv,
}

impl PhoxEngine {
    pub fn new() -> Self {
        let mut phox = PhoxEngine {
            ctx: TypeContext::new(),
            roots: RootModules::new(),
            global_symbol_env: SymbolEnv::new(),
            module_symbol_envs: HashMap::new(),
            impl_member_env: TraitMemberEnv::new(),
            impl_env: ImplEnv::new(),
        };
        let prelude = Module::new_root("prelude");
        phox.roots.add(prelude);
        let mut prelude = phox.roots.get("prelude").unwrap();
        phox.eval_mod(&mut prelude, PRELUDE).unwrap();

        // *** temporal implementation ***
        // currently user-root-module is just a clone of `prelude`.
        let usermod = phox.roots.get("prelude").unwrap().clone();
        usermod.borrow_mut().name = DEFAULT_USER_ROOT_MODULE_NAME.to_string();
        phox.roots.add(usermod);

        phox
    }
}

// -------------------------------------------------------------
/// Parse list of items.
pub fn parse_items(src: &str) -> Result<Vec<Item>, ParseError<usize, Token, LexicalError>> {
    let mut lexer = Lexer::new(src);
    ItemListParser::new().parse(&mut lexer)
}

/// Parse a program.
pub fn parse(src: &str) -> Result<Program, ParseError<usize, Token, LexicalError>> {
    parse_items(src)
}

/// Parse, infer type scheme, and evaluate of a program.
pub fn eval(src: &str) -> Result<(Value, TypeScheme), String> {
    let mut phox = PhoxEngine::new();
    phox.eval(src)
}

impl PhoxEngine {
    /// Get top-level SymbolEnv of the module.
    pub fn get_symbol_env(&mut self, module: &RefModule) -> SymbolEnv {
        let path = module.borrow().path();
        self.module_symbol_envs
            .entry(path)
            .or_insert_with(SymbolEnv::new)
            .clone()
    }

    /// Resolve list of items.
    pub fn resolve_items(&mut self, module: &RefModule, items: &mut Vec<Item>) -> Result<(), String> {
        let mut symbol_env = self.get_symbol_env(module);
        for mut item in items.iter_mut() {
            self.resolve_item(module, &mut symbol_env, &mut item)?;
        };
        Ok(())
    }

    /// Resolve an item.
    pub fn resolve_item(&mut self, module: &mut RefModule, symbol_env: &mut SymbolEnv, item: &mut Item) -> Result<(), String> {
        resolve_item(self, &mut module.borrow_mut(), symbol_env, item)
            .map_err(|e| format!("resolve error: {e}"))
    }

    /// Infer type scheme of an item.
    /// \note `item` must be resolved before.
    pub fn infer_item(&mut self, module: &mut RefModule, item: &mut Item) -> Result<TypeScheme, String> {
        // self.resolve_item(module, symbol_env, item)?;
        let ty = infer_item(self, &mut module.borrow_mut().icx, item)
            .map_err(|e| format!("infer error: {e}"))?;

        apply_trait_impls_item(self, &mut module.borrow_mut(), item)
            .map_err(|e| format!("infer error: {e}"))?;

        Ok(generalize(&mut self.ctx, &module.borrow_mut().icx, &ty))
    }

    /// Infer type scheme, and evaluate an item.
    /// \note `item` must be resolved before.
    pub fn eval_item(&mut self, module: &mut RefModule, item: &mut Item) -> Result<(Value, TypeScheme), String> {
        let sch = self.infer_item(module, item)?;
        let val = eval_item(&item, &mut module.borrow_mut().env);
        Ok((val, sch))
    }

    /// Parse, resolve, infer type scheme, and evaluate a program source code.
    pub fn eval_mod(&mut self, module: &mut RefModule, src: &str) -> Result<(Value, TypeScheme), String> {
        let mut items = parse(src).map_err(|e| format!("parse error: {e:?}"))?;
        self.resolve_items(module, &mut items)?;
        let mut last = None;
        for mut item in items {
            let res = self.eval_item(module, &mut item)?;
            last = Some(res);
        }
        last.ok_or_else(|| "program contained no expression".to_string())
    }

    pub fn eval(&mut self, src: &str) -> Result<(Value, TypeScheme), String> {
        let mut module = self.roots.get(DEFAULT_USER_ROOT_MODULE_NAME).unwrap();
        self.eval_mod(&mut module, src)
    }
}

// -------------------------------------------------------------
/// Parse an expression. (for test)
pub fn parse_expr(src: &str) -> Result<Expr, String> {
    let mut lexer = Lexer::new(src);
    ExprParser::new()
        .parse(&mut lexer)
        .map_err(|e| format!("parse error: {e:?}"))
}

// -------------------------------------------------------------
/// Infer type scheme of Expr AST. (for test)
pub fn infer_expr_scheme(ast: &mut Expr) -> Result<TypeScheme, String> {
    let mut phox = PhoxEngine::new();
    let mut module = phox.roots.get("prelude").unwrap();
    let mut symbol_env = SymbolEnv::new();
    let mut item = Item::Expr(ast.clone());
    phox.resolve_item(&mut module, &mut symbol_env, &mut item)?;
    phox.infer_item(&mut module, &mut item)
}

/// Infer type of Expr AST. (for test)
pub fn infer_expr_type(ast: &mut Expr) -> Result<Type, String> {
    let sch = infer_expr_scheme(ast)?;
    Ok(sch.target)
}

/// Parse and infer type scheme of an expression. (for test)
pub fn check_expr_scheme(src: &str) -> Result<TypeScheme, String> {
    let mut ast = parse_expr(src)?;
    infer_expr_scheme(&mut ast)
}

/// Parse and infer type of an expression. (for test)
pub fn check_expr_type(src: &str) -> Result<Type, String> {
    let sch = check_expr_scheme(src)?;
    Ok(sch.target)
}
